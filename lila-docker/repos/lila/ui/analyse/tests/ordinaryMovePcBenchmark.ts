import { StockfishWebEngine } from '../../lib/src/ceval/engines/stockfishWebEngine';
import { moveReviewEngineProfileSpec, moveReviewEngineCapability } from '../../lib/src/ceval/engines/moveReviewEngineProfiles';
import { makeMoveReviewWork, type MoveReviewWorkInput } from '../../lib/src/ceval/types';
import { features } from '../../lib/src/device';
import { Chess } from 'chessops/chess';
import { makeFen, parseFen } from 'chessops/fen';
import { parseUci } from 'chessops/util';
import { loadWholeGameDelivery } from './ordinaryMoveDeliveryBenchmark';

// Research entry point: import the real engine and protocol; do not copy their UCI parsing.
type Game = {id:string; title:string; initialFen:string; moves:string[]; san:string[]};
type Limits = {depth:number; nodes:number; movetimeMs:number};
const el = <T extends HTMLElement>(id:string) => document.getElementById(id)! as T;
const environment = {
  userAgent:navigator.userAgent, hardwareConcurrency:navigator.hardwareConcurrency,
  deviceMemory:(navigator as Navigator & {deviceMemory?:number}).deviceMemory ?? null,
  platform:navigator.platform, crossOriginIsolated, features:features(),
  screen:{width:screen.width,height:screen.height,pixelRatio:devicePixelRatio},
};
el('environment').textContent = JSON.stringify(environment,null,2);
let engine:StockfishWebEngine|undefined;
let stopReason:string|undefined;
let active = false;
let interactions: {atMs:number; eventDelayMs:number; nextFrameMs?:number}[] = [];
let started = 0;
let stopRequestedAt:number|undefined;
el('probe').onclick = event => {
  const click = {atMs:performance.now()-started,eventDelayMs:Math.max(0,performance.now()-event.timeStamp),nextFrameMs:undefined as number|undefined};
  interactions.push(click);
  el('clicks').textContent = `${interactions.length} interactions`;
  const then = performance.now();
  requestAnimationFrame(()=>click.nextFrameMs=performance.now()-then);
};
el('cancel').onclick = () => {stopRequestedAt=performance.now();stopReason='cancelled';engine?.stop();};
el('run').onclick = () => void run(false);
el('cancelTest').onclick = () => void run(true);

async function run(cancelTest:boolean) {
  if(active) return;
  active=true; stopReason=undefined; stopRequestedAt=undefined; interactions=[]; started=performance.now();
  el<HTMLButtonElement>('run').disabled=true; el<HTMLButtonElement>('cancelTest').disabled=true; el<HTMLButtonElement>('cancel').disabled=false;
  const strength=el<HTMLSelectElement>('strength').value;
  const totalLimitMs=Math.min(180,Math.max(1,Number(el<HTMLInputElement>('limit').value)))*1000;
  const limits:Limits = strength==='production' ? {depth:16,nodes:5_000_000,movetimeMs:5000} :
    {depth:99,nodes:strength==='more'?500_000:150_000,movetimeMs:5000};
  const extra:Limits = strength==='production' ? {depth:16,nodes:2_000_000,movetimeMs:2500} : {depth:99,nodes:strength==='more'?150_000:50_000,movetimeMs:2500};
  const multiPv=strength==='more'?3:6;
  const deadline=started+totalLimitMs;
  const ticks:number[]=[], frames:number[]=[], longTasks:{startTime:number;duration:number}[]=[];
  let lastTick=performance.now(),lastFrame=lastTick,raf=0;
  const tick=setInterval(()=>{const now=performance.now();ticks.push(now-lastTick);lastTick=now;},25);
  const frame=(now:number)=>{frames.push(now-lastFrame);lastFrame=now;raf=requestAnimationFrame(frame);};
  raf=requestAnimationFrame(frame);
  const observer = PerformanceObserver.supportedEntryTypes.includes('longtask') ? new PerformanceObserver(list=>list.getEntries().forEach(x=>longTasks.push({startTime:x.startTime-started,duration:x.duration}))) : undefined;
  observer?.observe({type:'longtask',buffered:false});
  const integrated = new URLSearchParams(location.search).has('delivery');
  const report:any={harnessRevision:4,id:`${integrated?'integrated-':''}${cancelTest?'cancel':strength}-${Date.now()}`,startedAt:new Date().toISOString(),environment,
    settings:{oneEngineInstance:true,threads:2,hashMB:16,multiPv,limits,extra,targetMs:120000,totalLimitMs,clearHashEverySearch:true,history:'initial FEN plus full prefix; no FEN deduplication'},
    jobs:[], positions:[], commands:[], errors:[], firstResultMs:null,firstCompletedSearchMs:null,firstCompletedPlyMs:null};
  const timer=setTimeout(()=>{stopRequestedAt=performance.now();stopReason='budget_exhausted';engine?.stop();},totalLimitMs);
  let currentJob:any;
  try {
    const games:Game[]=await (await fetch('pc-games.json')).json();
    const game=games.find(x=>x.id===el<HTMLSelectElement>('game').value)!;
    report.game={id:game.id,title:game.title,plies:game.moves.length,initialFen:game.initialFen};
    const board=Chess.fromSetup(parseFen(game.initialFen).unwrap()).unwrap();
    const positions=game.moves.map((uci,index)=>{
      const move=parseUci(uci)!;
      if(!board.isLegal(move)) throw Error(`Illegal source move ${index+1}: ${uci}`);
      const position={ply:index+1,fen:makeFen(board.toSetup()),move:uci,san:game.san[index],history:game.moves.slice(0,index),legalMoves:Array.from(board.allDests()).reduce((sum,[from,tos])=>sum+Array.from(tos).reduce((count,to)=>count+(board.board.pawn.has(from)&&(to<8||to>=56)?4:1),0),0),status:'notsearched',jobs:[] as number[]};
      board.play(move); return position;
    });
    report.positions=positions;
    report.finalFen=makeFen(board.toSetup());
    const delivery = integrated ? await loadWholeGameDelivery(game.id, positions.length) : undefined;
    if (delivery) report.delivery = delivery.report;
    el<HTMLProgressElement>('progress').max=positions.length;
    el<HTMLProgressElement>('progress').value=0;
    const capability=moveReviewEngineCapability({features:features(),hardwareConcurrency:navigator.hardwareConcurrency});
    if(!capability.supported) throw Error(`Browser unsupported: ${capability.reason}`);
    el('status').textContent='Loading browser engine…';
    const bootStart=performance.now();
    engine=new StockfishWebEngine(capability.info,status=>{if(status?.error)report.errors.push(status.error);});
    if(!await engine.ready) throw Error(`Engine failed to load: ${engine.failed}`);
    report.bootMs=performance.now()-bootStart;
    report.engineName=engine.engineName();
    report.manifest=moveReviewEngineProfileSpec();
    const uci=engine.module!.uci.bind(engine.module);
    engine.module!.uci=(command:string)=>{report.commands.push({atMs:performance.now()-started,job:currentJob?.id??null,command});return uci(command);};
    const listen=engine.module!.listen;
    engine.module!.listen=(line:string)=>{
      if(currentJob && line.startsWith('info ')) {
        currentJob.rawReportedNodes=Math.max(currentJob.rawReportedNodes??0,Number(line.match(/\bnodes (\d+)/)?.[1]??0));
        currentJob.rawReportedEngineMs=Math.max(currentJob.rawReportedEngineMs??0,Number(line.match(/\btime (\d+)/)?.[1]??0));
      }
      listen(line);
    };
    async function search(position:typeof positions[number],restricted:boolean,searchLimits:Limits) {
      if(stopReason || performance.now()>=deadline) {stopReason??='budget_exhausted';return undefined;}
      const job:any={id:report.jobs.length,ply:position.ply,kind:restricted?'played':'root',limits:searchLimits,multiPv:restricted?1:Math.min(multiPv,position.legalMoves),rootMoves:restricted?[position.move]:[],startedMs:performance.now()-started,reportedNodes:0,reportedEngineMs:0,firstEmissionMs:null,emissions:0};
      report.jobs.push(job);position.jobs.push(job.id);currentJob=job;
      const searchStart=performance.now();
      const result=await new Promise<Tree.LocalEval>((resolve,reject)=>{
        const watchdog=setTimeout(()=>{engine?.destroy();reject(Error('Engine did not acknowledge stop within 3 seconds of search/time deadline'));},Math.min(searchLimits.movetimeMs,Math.max(0,deadline-performance.now()))+3000);
        const work=makeMoveReviewWork(capability.profile,{
          variant:'standard',path:String(position.ply),ply:position.ply-1,initialFen:game.initialFen,currentFen:position.fen,moves:position.history,
          multiPv:job.multiPv,searchLimits:searchLimits as MoveReviewWorkInput['searchLimits'],rootMoves:job.rootMoves,
          observe:(nodes,millis)=>{job.reportedNodes=Math.max(job.reportedNodes,nodes);job.reportedEngineMs=Math.max(job.reportedEngineMs,millis);},
          emit:evaluation=>{
            job.emissions++;job.firstEmissionMs??=performance.now()-searchStart;
            if(evaluation.pvs.length) report.firstResultMs??=performance.now()-started;
            if(evaluation.bestmove) {clearTimeout(watchdog);resolve(structuredClone(evaluation));}
          },
        });
        engine!.start(work);
        if(cancelTest) setTimeout(()=>{stopRequestedAt=performance.now();stopReason='cancelled';engine?.stop();},250);
      });
      job.wallMs=performance.now()-searchStart;
      job.completedMs=performance.now()-started;
      job.status=stopReason??(result.pvs.length?'completed':'no_complete_pv');
      job.result=result;
      if(stopRequestedAt!==undefined) report.stopAcknowledgedMs=performance.now()-stopRequestedAt;
      report.firstCompletedSearchMs??=performance.now()-started;
      currentJob=undefined;
      return job;
    }
    for(const position of positions) {
      if(stopReason) break;
      delivery?.show(position.ply);
      el('status').textContent=`Analysing ply ${position.ply}/${positions.length}: ${position.san} · ${((performance.now()-started)/1000).toFixed(1)} seconds`;
      const root=await search(position,false,cancelTest?{depth:99,nodes:100_000_000,movetimeMs:60000}:limits);
      if(!root) break;
      position.status=root.status;
      if(root.status!=='completed') {if(stopReason)break;continue;}
      if(!root.result.pvs.some((pv:Tree.PvData)=>pv.moves[0]===position.move)) {
        const played=await search(position,true,extra);
        if(!played) {position.status='played_notsearched';break;}
        position.status=played.status;
      }
      if(position.status==='completed') report.firstCompletedPlyMs??=performance.now()-started;
      el<HTMLProgressElement>('progress').value=position.ply;
    }
    report.status=stopReason??(positions.every(p=>p.status==='completed')?'completed':'partial');
  } catch(error) {report.status='failed';report.errors.push(String(error));}
  finally {
    clearTimeout(timer);clearInterval(tick);cancelAnimationFrame(raf);observer?.disconnect();
    const disposeStart=performance.now();engine?.destroy();engine=undefined;
    report.destroyMs=performance.now()-disposeStart;
    report.totalMs=performance.now()-started;
    const distribution=(values:number[])=>{const sorted=[...values].sort((a,b)=>a-b);return {samples:sorted.length,p50:sorted[Math.floor(sorted.length*.5)]??null,p95:sorted[Math.floor(sorted.length*.95)]??null,p99:sorted[Math.floor(sorted.length*.99)]??null,max:sorted.at(-1)??null};};
    report.responsiveness={interval25ms:distribution(ticks),animationFrames:distribution(frames),longTasks,interactions,visibility:document.visibilityState};
    report.summary={positions:report.positions.length,completed:report.positions.filter((p:any)=>p.status==='completed').length,notsearched:report.positions.filter((p:any)=>p.status==='notsearched').length,jobs:report.jobs.length,reportedNodes:report.jobs.reduce((a:number,j:any)=>a+j.reportedNodes,0),reportedEngineMs:report.jobs.reduce((a:number,j:any)=>a+j.reportedEngineMs,0),wallSearchMs:report.jobs.reduce((a:number,j:any)=>a+(j.wallMs??0),0)};
    report.summary.rawEngineReportedNodes=report.jobs.reduce((a:number,j:any)=>a+(j.rawReportedNodes??0),0);
    report.summary.rawEngineReportedMs=report.jobs.reduce((a:number,j:any)=>a+(j.rawReportedEngineMs??0),0);
    report.resources=performance.getEntriesByType('resource').filter(x=>x.startTime>=started).map(x=>{const r=x as PerformanceResourceTiming;return {name:r.name,duration:r.duration,transferSize:r.transferSize,encodedBodySize:r.encodedBodySize};});
    try {const saved=await fetch('/pc-result',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(report)});if(!saved.ok)throw Error(`Save failed ${saved.status}`);report.saved=await saved.json();}catch(error){report.saveError=String(error);}
    el('status').textContent=`${report.status}: ${report.summary.completed}/${report.summary.positions} plies · ${(report.totalMs/1000).toFixed(2)} seconds`;
    el('result').textContent=JSON.stringify({id:report.id,status:report.status,summary:report.summary,bootMs:report.bootMs,firstResultMs:report.firstResultMs,firstCompletedPlyMs:report.firstCompletedPlyMs,totalMs:report.totalMs,stopAcknowledgedMs:report.stopAcknowledgedMs,responsiveness:report.responsiveness,errors:report.errors,saved:report.saved,saveError:report.saveError},null,2);
    active=false;el<HTMLButtonElement>('run').disabled=false;el<HTMLButtonElement>('cancelTest').disabled=false;el<HTMLButtonElement>('cancel').disabled=true;
  }
}
