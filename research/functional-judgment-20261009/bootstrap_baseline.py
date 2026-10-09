"""Copy the previous verified baseline without modifying its owning checkout."""
from datetime import datetime, timezone, timedelta
import hashlib
import json
from pathlib import Path
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
SOURCE = Path(r"C:\Users\양자\.codex\worktrees\8f44\Chesstories")
assert ROOT.name == SOURCE.name and ROOT != SOURCE
assert ROOT.parent.name == "functional-choice-value"
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
prior_manifest = SOURCE / "target/relationship-delivery-20261004/main-manifest-meaningv40a.json"
manifest = json.loads(prior_manifest.read_text(encoding="utf8"))
for name, digest in manifest["source_sha256"].items():
    assert sha(SOURCE / name) == digest, name

files = subprocess.check_output(
    ["git", "ls-files", "--modified", "--others", "--exclude-standard", "-z"], cwd=SOURCE
).decode("utf8").split("\0")
files = sorted({name for name in files if name and (SOURCE / name).is_file()
                and name.split("/")[0] in {"chesstory-runtime", "judgment-evaluation", "lila-docker", "research"}})
copied = []
for name in files:
    src, dst = SOURCE / name, ROOT / name
    digest = sha(src)
    assert not dst.exists() or sha(dst) == digest or name.split("/")[0] != "research", name
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)
    assert sha(src) == digest == sha(dst), name
    copied.append({"path": name, "sha256": digest, "bytes": src.stat().st_size})
for name, digest in manifest["source_sha256"].items():
    assert sha(ROOT / name) == digest, name

support = [
    "target/annotated-games-20261003/classpath.json",
    "target/relationship-delivery-20261004/compile.py",
    "chesstory-runtime/target/streams/test/dependencyClasspath/_global/streams/export",
]
for name in support:
    src, dst = SOURCE / name, ROOT / name
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)
    assert sha(src) == sha(dst)

compiled = ROOT / "target/relationship-delivery-20261004"
classes = compiled / "classes-valuebase40a"
assert not classes.exists()
shutil.copytree(SOURCE / "target/relationship-delivery-20261004/classes-meaningv40a", classes)
for name, digest in manifest["class_sha256"].items():
    assert sha(classes / name) == digest, name
cp = json.loads((SOURCE / "target/relationship-delivery-20261004/classpath-meaningv40a.json").read_text(encoding="utf8"))
cp["runtime_classpath"][0] = str(classes)
manifest["version"] = "valuebase40a"
(compiled / "main-manifest-valuebase40a.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=2), encoding="utf8")
(compiled / "classpath-valuebase40a.json").write_text(json.dumps(cp, ensure_ascii=False, indent=2), encoding="utf8")

now = datetime.now(timezone.utc)
record = {
    "started_utc": now.isoformat(), "maximum_hours": 15,
    "deadline_utc": (now + timedelta(hours=15)).isoformat(),
    "source_checkout": str(SOURCE), "owned_checkout": str(ROOT),
    "source_is_read_only": True, "software_baseline": "meaningv40a",
    "prior_main_manifest_sha256": sha(prior_manifest), "copied": copied,
    "source_count_verified": len(manifest["source_sha256"]),
    "compiled_class_count_verified": len(manifest["class_sha256"]),
    "historical_record_contents_rewritten": False,
    "dependency_jars": "Existing read-only Coursier cache paths are reused; new outputs belong to this checkout.",
}
dest = HERE / "baseline-copy-manifest.json"
assert not dest.exists()
dest.write_text(json.dumps(record, ensure_ascii=False, indent=2) + "\n", encoding="utf8")
print(json.dumps({"copied_files": len(copied), "copied_bytes": sum(p["bytes"] for p in copied),
                  "verified_sources": record["source_count_verified"],
                  "verified_classes": record["compiled_class_count_verified"],
                  "manifest": str(dest)}, ensure_ascii=False))
