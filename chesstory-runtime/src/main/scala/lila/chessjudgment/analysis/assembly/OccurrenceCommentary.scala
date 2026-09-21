package lila.chessjudgment.analysis.assembly

import lila.chessjudgment.model.judgment.*
import lila.chessjudgment.analysis.line.LineFactNormalizer
import lila.chessjudgment.analysis.position.PositionRelationExtractor.ClosedRelationAbsenceQuery

enum PieceControlState:
  case Present, Absent, Unknown, Removed

final case class PieceControlPoint private[assembly] (
    controller: Option[RelationColoredPieceWitness],
    target: Option[RelationColoredPieceWitness],
    state: PieceControlState,
    premises: List[EvidenceRef]
)

enum MoveObservationMeaning:
  case Capture(role: EvidencePieceRole, square: EvidenceSquare, recapture: LineMaterialRecaptureStatus)
  case Development(centralSquares: List[EvidenceSquare])
  case PieceControl(before: PieceControlPoint, after: PieceControlPoint)
  case Castling(rook: RelationMoveTransitionWitness)
  case Promotion(role: EvidencePieceRole)
  case StructuralChange(consequences: List[TransitionConsequence])
  case CertifiedRelation(detail: RelationWitnessDetail.CaptureRecaptureInventory |
    RelationWitnessDetail.CreatedCheckResponseInventory | RelationWitnessDetail.RootCheckResponse |
    RelationWitnessDetail.PawnTopologyTransition | RelationWitnessDetail.StalemateTransition)

final case class MoveObservation private[assembly] (
    transition: MoveTransitionEdge,
    mover: RelationMoveTransitionWitness,
    meaning: MoveObservationMeaning,
    premises: List[EvidenceRef]
)

/** Content selection over independently certified occurrences; no score-derived chess claims. */
object OccurrenceCommentary:
  def fromAssembly(ctx: JudgmentAssemblyContext): List[MoveObservation] =
    fromAssembly(ctx, ctx.transitions.iterator.map(_.evidence.id).toSet)

  /** Selects exact transitions without opening unrelated occurrence observations.
    * Each selected endpoint still supplies its complete certified control inventory.
    */
  def fromAssembly(ctx: JudgmentAssemblyContext, occurrenceIds: Set[String]): List[MoveObservation] =
    val selected = ctx.transitions.filter(edge => occurrenceIds(edge.evidence.id))
    if selected.isEmpty then return Nil
    val selectedIds = selected.iterator.map(_.evidence.id).toSet
    val selectedPositions = selected.iterator.flatMap(edge => Iterator(edge.from, edge.to)).toSet
    val selectedTransitions = selected.iterator.map(edge => (edge.role, edge.from, edge.moveUci, edge.to)).toSet
    val graph = ctx.evidenceGraph
    val records = graph.byId.valuesIterator.filter(record => selectedPositions(record.ref.position)).toList
    val owners = records.collect {
      case EvidenceRecord(ref, owner: ClosedRelationOccurrenceEvidence, _) if selectedIds(owner.edge.evidence.id) => ref.id -> owner
    }.toMap
    val certified = records.flatMap { record =>
      record.payload match
        case relation: RelationFactEvidence =>
          val meaning = relation.detail match
            case value: RelationWitnessDetail.CaptureRecaptureInventory => Some(MoveObservationMeaning.CertifiedRelation(value))
            case value: RelationWitnessDetail.CreatedCheckResponseInventory => Some(MoveObservationMeaning.CertifiedRelation(value))
            case value: RelationWitnessDetail.RootCheckResponse => Some(MoveObservationMeaning.CertifiedRelation(value))
            case value: RelationWitnessDetail.PawnTopologyTransition => Some(MoveObservationMeaning.CertifiedRelation(value))
            case value: RelationWitnessDetail.StalemateTransition => Some(MoveObservationMeaning.CertifiedRelation(value))
            case _ => None
          meaning.toList.flatMap(value => record.parents.flatMap(parent =>
            owners.get(parent.id).flatMap(owner => owner.outputFor(record.ref).filter(_ => graph.proofEligible(record)).map(_ =>
              owner.edge.evidence.id -> (record.ref, value)))))
        case _ => Nil
    }.toList.groupMap(_._1)(_._2)
    val incoming = ctx.transitions.map(edge => edge.to -> edge).toMap
    val structural = records.collect {
      case record @ EvidenceRecord(_, delta: StructuralDeltaEvidence, _)
          if selectedTransitions((delta.role, delta.from, delta.moveUci, delta.to)) && graph.proofEligible(record) =>
        (delta.role, delta.from, delta.moveUci, delta.to) -> (record.ref, delta.consequences)
    }.toMap
    val controls = records.collect {
      case record @ EvidenceRecord(_, relation: RelationFactEvidence, _)
          if relation.kind == RelationFactKind.GeometricControl => record
    }.toList.groupBy(_.ref.position)
    val positions = records.collect {
      case record @ EvidenceRecord(ref, PositionOccurrenceEvidence(value), _)
          if ctx.positionAnalyses.get(ref.position).exists(_.occurrence == value) && graph.proofEligible(record) => record
    }.foldLeft(Map.empty[PositionNodeRef, EvidenceRecord]) { (known, record) =>
      if known.contains(record.ref.position) then known else known.updated(record.ref.position, record)
    }
    // Share each occurrence's certified controls across every incoming/outgoing transition.
    // Occupancy makes the endpoint a piece, rather than a square that a different piece may later occupy.
    val pieceControls = controls.map { case (position, records) =>
      val values = records.flatMap { record =>
        Option.when(positions.contains(position) && graph.proofEligible(record))(record.payload).flatMap {
          case relation: RelationFactEvidence => relation.detail match
            case RelationWitnessDetail.GeometricControl(side, from, role, to) =>
              ctx.positionAnalyses(position).relationInventory.occupantAt(to).map { target =>
                (RelationColoredPieceWitness(from, role, side) -> target) -> record
              }
            case _ => None
          case _ => None
        }
      }.toMap
      position -> values
    }
    val history = ctx.input.positionHistory
    val knownInitial = history.initialFen == chess.variant.Standard.initialFen.value
    val touched = scala.collection.mutable.Map.empty[PositionNodeRef, Set[String]]
    ctx.root.foreach(root => touched(root) = history.segmentReplaySteps.flatMap(step =>
      List(step.uci.take(2), step.uci.slice(2, 4))).toSet)
    ctx.transitions.flatMap { edge =>
      val previousTouches = touched.getOrElse(edge.from, Set.empty)
      touched(edge.to) = previousTouches ++ Set(edge.moveUci.take(2), edge.moveUci.slice(2, 4))
      if !selectedIds(edge.evidence.id) ||
          !graph.record(edge.evidence).exists(record => edge.matches(record) && graph.proofEligible(record)) then Nil
      else ctx.transitionReplays.get(edge.evidence.id).toList.flatMap { replay =>
        val (canonical, single) = replay.singleStepReplays.head
        val root = canonical.relationDelta.rootMove
        val previous = incoming.get(edge.from).flatMap(prior => ctx.transitionReplays.get(prior.evidence.id))
          .map(_.singleStepReplays.head)
        val capture = root.capture.map { captured =>
          val status = LineFactNormalizer.recaptureStatus(history, ctx.predecessorReplay, previous,
            single, canonical.declared, if ctx.root.contains(edge.from) then 0 else 1)
          MoveObservation(edge, root.witness,
            MoveObservationMeaning.Capture(captured.capturedRole, captured.capturedSquare, status),
            edge.evidence :: (status match
              case LineMaterialRecaptureStatus.Proven(_) => incoming.get(edge.from).toList.map(_.evidence)
              case _ => Nil))
        }
        val home = if root.side.white then Set("b1", "c1", "f1", "g1") else Set("b8", "c8", "f8", "g8")
        val firstDeparture = knownInitial && Set("knight", "bishop")(root.beforeRole.name) &&
          home(root.from.key) && !home(root.to.key) && !previousTouches(root.from.key)
        val development = Option.when(firstDeparture) {
          val centralControls = controls.getOrElse(edge.to, Nil).filter { record =>
            record.payload match
              case relation: RelationFactEvidence => relation.detail match
                case RelationWitnessDetail.GeometricControl(side, controller, role, target) =>
                  side == root.side && controller == root.to && role == root.afterRole &&
                    Set("d4", "e4", "d5", "e5")(target.key) && graph.proofEligible(record)
                case _ => false
              case _ => false
          }
          val squares = centralControls.collect {
            case EvidenceRecord(_, relation: RelationFactEvidence, _) => relation.detail match
              case RelationWitnessDetail.GeometricControl(_, _, _, target) => target
              case _ => throw IllegalStateException("filtered control lost its kind")
          }.distinct.sortBy(_.key)
          MoveObservation(edge, root.witness, MoveObservationMeaning.Development(squares),
            edge.evidence :: centralControls.map(_.ref))
        }
        val beforeControls = pieceControls.getOrElse(edge.from, Map.empty)
        val afterControls = pieceControls.getOrElse(edge.to, Map.empty)
        val footprint = canonical.boardFootprint
        val pairs = (beforeControls.keys.toList ++ afterControls.keys.flatMap { case (controller, target) =>
          for
            previousController <- controller.before(footprint)
            previousTarget <- target.before(footprint)
          yield previousController -> previousTarget
        }).distinct.sortBy { case (controller, target) =>
          (controller.side.toString, controller.square.key, target.square.key)
        }
        def point(position: PositionNodeRef, controller: Option[RelationColoredPieceWitness],
            target: Option[RelationColoredPieceWitness]): PieceControlPoint =
          val known = positions.get(position)
          val positive = for
            from <- controller
            to <- target
            record <- pieceControls.getOrElse(position, Map.empty).get(from -> to)
          yield record
          val (state, premises) = if controller.isEmpty || target.isEmpty then
            PieceControlState.Removed -> List(edge.evidence)
          else positive match
            case Some(record) => PieceControlState.Present -> (record.ref :: known.toList.map(_.ref))
            case None =>
              val absent = for
                occurrence <- known
                analysis <- ctx.positionAnalyses.get(position)
                from <- controller
                to <- target
                if analysis.relationInventory.occupantAt(from.square).contains(from) &&
                  analysis.relationInventory.occupantAt(to.square).contains(to)
                certificate <- analysis.relationInventory.certifyAbsence(
                  ClosedRelationAbsenceQuery.GeometricControlFromTo(from.side,
                    RelationPieceWitness(from.square, from.role), to.square))
                proof <- analysis.relationInventory.bindAbsence(certificate, position, occurrence.ref.scope)
              yield proof
              if absent.nonEmpty then PieceControlState.Absent -> known.toList.map(_.ref)
              else PieceControlState.Unknown -> Nil
          PieceControlPoint(controller, target, state, premises)
        val controlObservations = pairs.flatMap { case (controller, target) =>
          val before = point(edge.from, Some(controller), Some(target))
          val after = point(edge.to, controller.after(footprint), target.after(footprint))
          Option.when(before.state != after.state || before.controller != after.controller || before.target != after.target) {
            MoveObservation(edge, root.witness, MoveObservationMeaning.PieceControl(before, after),
              (edge.evidence :: (before.premises ++ after.premises)).distinct)
          }
        }
        val existingRelations = certified.getOrElse(edge.evidence.id, Nil).sortBy(_._1.id).map { case (ref, meaning) =>
          MoveObservation(edge, root.witness, meaning, List(edge.evidence, ref))
        }
        val structure = structural.get((edge.role, edge.from, edge.moveUci, edge.to)).toList.map { case (ref, consequences) =>
          MoveObservation(edge, root.witness, MoveObservationMeaning.StructuralChange(consequences), List(edge.evidence, ref))
        }
        val castling = Option.when(root.isCastling)(footprint.pieceTransitions.find(_.beforeRole == chess.Rook)).flatten.map { rook =>
          MoveObservation(edge, root.witness, MoveObservationMeaning.Castling(RelationMoveTransitionWitness.from(rook)), List(edge.evidence))
        }
        val promotion = Option.when(root.beforeRole != root.afterRole) {
          MoveObservation(edge, root.witness, MoveObservationMeaning.Promotion(root.afterRole), List(edge.evidence))
        }
        capture.toList ++ development.toList ++ castling.toList ++ promotion.toList ++ controlObservations ++ existingRelations ++ structure
      }
    }
