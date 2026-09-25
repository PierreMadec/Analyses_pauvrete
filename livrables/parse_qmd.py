import re
import json

with open("../insee_référence.qmd", encoding="utf-8") as f:
    content = f.read()

content = re.sub(r"^---.*?---\n", "", content, flags=re.S)
content = re.sub(r"```\{r setup.*?```\n", "", content, flags=re.S)

FIG_MAP = {
    "fig-emploi-pauvrete": "01_emploi_pauvrete",
    "fig-rel-anc-trav": "02_relatif_ancre",
    "fig-tp3-taux-config": "03_taux_par_config",
    "fig-tp-diag": "04_diagnostic_causes",
    "fig-salaire-distrib": "05_distribution_salaire_horaire",
    "fig-tp-cf": "06_contrefactuels",
    "fig-oaxaca-barre": "07_oaxaca_composition_coefficients",
    "fig-cohortes-jeunes": "08_cohortes",
    "fig-soutien-amortisseur": "09_effet_amortisseur",
    "fig-decompo-transferts-config": "10_decompo_transferts",
}


def classify(text):
    if text.startswith("## "):
        return {"type": "h2", "text": text[3:].strip()}
    if text.startswith("# "):
        return {"type": "h1", "text": text[2:].strip()}
    if text.startswith(":::"):
        return None
    if text.startswith("- "):
        items = [l[2:].strip() for l in text.split("\n") if l.strip().startswith("- ")]
        return {"type": "bullets", "items": items}
    return {"type": "para", "text": text}


def process_text_segment(seg, blocks):
    callout_m = re.search(r'::: \{\.callout-note title="(.*?)"\}\n(.*?)\n:::', seg, flags=re.S)
    if callout_m:
        before = seg[: callout_m.start()]
        after = seg[callout_m.end():]
        for b in re.split(r"\n\n+", before.strip()):
            if b.strip():
                c = classify(b.strip())
                if c:
                    blocks.append(c)
        blocks.append({"type": "callout", "title": callout_m.group(1), "text": callout_m.group(2).strip()})
        for a in re.split(r"\n\n+", after.strip()):
            if a.strip():
                c = classify(a.strip())
                if c:
                    blocks.append(c)
        return
    for para in re.split(r"\n\n+", seg.strip()):
        para = para.strip()
        if para:
            c = classify(para)
            if c:
                blocks.append(c)


blocks = []
parts = re.split(r"(```\{r\}.*?```)", content, flags=re.S)

for part in parts:
    part = part.strip("\n")
    if not part.strip():
        continue
    if part.startswith("```{r}"):
        label_m = re.search(r"#\|\s*label:\s*(\S+)", part)
        label = label_m.group(1) if label_m else None
        img = FIG_MAP.get(label)
        if img:
            blocks.append({"type": "image", "file": img})
        continue
    process_text_segment(part, blocks)

with open("parsed_blocks.json", "w", encoding="utf-8") as f:
    json.dump(blocks, f, ensure_ascii=False, indent=2)

print(f"{len(blocks)} blocks written to parsed_blocks.json")
for b in blocks:
    print(" -", b["type"], "|", (b.get("text") or b.get("title") or b.get("file") or "")[:60])
