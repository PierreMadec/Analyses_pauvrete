#!/usr/bin/env python3
"""
Construit le manuscrit Word de soumission (insee_référence_manuscrit.docx) à
partir de insee_référence.qmd.

Conventions du manuscrit, reprises de la version précédente :
  - deux modes de rendu des figures :
      * --figures appels (défaut) : chaque figure devient un appel
        [Figure N — <légende sans la lecture>. Données, unité et habillage :
         fichier tableur, onglet « Figure N ».]
      * --figures images : les graphiques sont rendus en PNG depuis les .rds
        du dossier figure/ et incorporés au document, légende complète en
        dessous. Utile pour la relecture, pas pour la soumission.
  - les renvois @fig-xxx deviennent « figure N »
  - les encadrés (callout-note) deviennent un titre en gras suivi du corps
  - les styles Word proviennent du document de référence passé en --reference-doc

Usage : python3 livrables/build_manuscrit.py [-o sortie.docx]
"""
import argparse
import re
import shutil
import subprocess
import sys
from pathlib import Path

RACINE = Path(__file__).resolve().parent.parent
QMD = RACINE / "insee_référence.qmd"
REF = RACINE / "insee_référence_manuscrit.docx"
BOILERPLATE = "Données, unité et habillage : fichier tableur, onglet « Figure {n} »."


def decouper_yaml(texte):
    m = re.match(r"^---\n(.*?)\n---\n", texte, re.S)
    if not m:
        sys.exit("En-tête YAML introuvable.")
    return m.group(1), texte[m.end():]


def champ_yaml(yaml, cle):
    m = re.search(rf"^{cle}:\s*\"?(.*?)\"?\s*$", yaml, re.M)
    return m.group(1) if m else None


def abstract_yaml(yaml):
    """Le résumé n'est repris que s'il est renseigné (abstract: "" = pas de résumé)."""
    m = re.search(r"^abstract:\s*\|\n(.*?)(?=\n[a-z-]+:)", yaml, re.S | re.M)
    if not m:
        return None
    texte = " ".join(l.strip() for l in m.group(1).split("\n") if l.strip())
    return texte or None


def legende_courte(cap):
    """Légende jusqu'à « Lecture : » — c'est ce qui sert d'appel de figure."""
    cap = cap.strip().strip('"')
    return re.split(r"\s*Lecture\s*:", cap)[0].strip()


def details_chunk(chunk):
    """.rds affiché et hauteur du graphique, lus dans l'appel show_girafe."""
    rds = re.search(r'"([\w./-]+\.rds)"', chunk)
    # les chunks écrivent file.path(path_g, "x.rds") : path_g vaut "figure"
    haut = re.search(r"height_svg\s*=\s*([\d.]+)", chunk)
    chemin = rds.group(1) if rds else None
    if chemin and "/" not in chemin:
        chemin = f"figure/{chemin}"
    return chemin, float(haut.group(1)) if haut else 5.5


def collecter_figures(corps):
    """Retourne [(label, légende courte, légende complète, rds, hauteur)]."""
    figs = []
    for chunk in re.findall(r"^```\{r[^}]*\}\n(.*?)^```", corps, re.S | re.M):
        lab = re.search(r"^#\|\s*label:\s*(fig-[\w-]+)\s*$", chunk, re.M)
        if not lab:
            continue
        cap = re.search(r'^#\|\s*fig-cap:\s*(.*)$', chunk, re.M)
        if not cap:
            sys.exit(f"Figure sans fig-cap : {lab.group(1)}")
        entier = cap.group(1).strip().strip('"')
        rds, haut = details_chunk(chunk)
        figs.append((lab.group(1), legende_courte(cap.group(1)), entier, rds, haut))
    return figs


def rendre_png(figs, dossier):
    """Rend chaque .rds en PNG via Rscript. Retourne {label: chemin}."""
    dossier.mkdir(parents=True, exist_ok=True)
    lignes, chemins = [], {}
    for i, (lab, _, _, rds, haut) in enumerate(figs, start=1):
        if rds is None:
            sys.exit(f"Impossible de localiser le .rds de {lab}")
        png = dossier / f"{i:02d}_{lab}.png"
        chemins[lab] = png
        lignes.append(f'{RACINE / rds}\t{png}\t{haut}')
    script = (
        'suppressMessages(library(ggplot2))\n'
        'for (l in readLines(commandArgs(TRUE)[1])) {\n'
        '  p <- strsplit(l, "\\t")[[1]]\n'
        '  ggsave(p[2], readRDS(p[1]), width = 10, height = as.numeric(p[3]),\n'
        '         dpi = 150, bg = "white", limitsize = FALSE)\n'
        '}\n')
    manifeste = dossier / "_manifeste.tsv"
    manifeste.write_text("\n".join(lignes) + "\n", encoding="utf-8")
    script_r = dossier / "_rendu.R"
    script_r.write_text(script, encoding="utf-8")
    res = subprocess.run(["Rscript", str(script_r), str(manifeste)],
                         capture_output=True, text=True)
    if res.returncode:
        sys.exit("Rendu des figures échoué :\n" + res.stderr)
    manifeste.unlink()
    script_r.unlink()
    return chemins


def transformer(corps, numeros, images=None):
    def remplacer_chunk(m):
        chunk = m.group(1)
        lab = re.search(r"^#\|\s*label:\s*(fig-[\w-]+)\s*$", chunk, re.M)
        if not lab:
            return ""  # setup et chunks techniques : supprimés
        lab = lab.group(1)
        n = numeros[lab]
        brut = re.search(r'^#\|\s*fig-cap:\s*(.*)$', chunk, re.M).group(1)
        if images:
            entier = brut.strip().strip('"')
            return (f"![]({images[lab].as_posix()}){{width=16cm}}\n\n"
                    f"**Figure {n} —** {entier}")
        cap = legende_courte(brut)
        if not cap.endswith("."):
            cap += "."
        return f"[Figure {n} — {cap} {BOILERPLATE.format(n=n)}]"

    corps = re.sub(r"^```\{r[^}]*\}\n(.*?)^```", remplacer_chunk, corps, flags=re.S | re.M)
    corps = re.sub(r"\(@(fig-[\w-]+)\)",
                   lambda m: f"(figure {numeros[m.group(1)]})", corps)
    corps = re.sub(r"@(fig-[\w-]+)",
                   lambda m: f"figure {numeros[m.group(1)]}", corps)
    corps = re.sub(r'^:::\s*\{\.callout-note title="(.*?)"\}\s*$',
                   lambda m: f"**{m.group(1)}**\n", corps, flags=re.M)
    corps = re.sub(r"^:::\s*$", "", corps, flags=re.M)
    corps = re.sub(r"\n{3,}", "\n\n", corps)
    return corps.strip()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("-o", "--out", default=str(RACINE / "insee_référence_manuscrit.docx"))
    ap.add_argument("--reference-doc", default=str(REF))
    ap.add_argument("--md", help="conserver le markdown intermédiaire à ce chemin")
    ap.add_argument("--figures", choices=["appels", "images"], default="appels",
                    help="appels = renvois au fichier tableur (soumission) ; "
                         "images = graphiques incorporés (relecture)")
    args = ap.parse_args()

    yaml, corps = decouper_yaml(QMD.read_text(encoding="utf-8"))
    figs = collecter_figures(corps)
    numeros = {lab: i + 1 for i, (lab, *_) in enumerate(figs)}
    images = rendre_png(figs, RACINE / "livrables" / "images_manuscrit") \
        if args.figures == "images" else None

    titre = champ_yaml(yaml, "title")
    resume = abstract_yaml(yaml)
    entete = ["Pierre Madec (OFCE, Sciences Po Paris)", ""]
    if resume:
        entete += [resume, ""]

    md = "\n".join(entete) + "\n" + transformer(corps, numeros, images) + "\n"
    chemin_md = Path(args.md) if args.md else Path(args.out).with_suffix(".md")
    chemin_md.write_text(md, encoding="utf-8")

    pandoc = shutil.which("pandoc")
    if pandoc is None:
        # Quarto embarque son propre pandoc : on le réutilise.
        for c in Path("/Applications/quarto/bin/tools").glob("*/pandoc"):
            pandoc = str(c)
            break
    if pandoc is None:
        sys.exit("pandoc introuvable (ni sur le PATH, ni dans l'installation Quarto).")

    cmd = [pandoc, str(chemin_md), "-o", args.out, "--from", "markdown",
           "--metadata", f"title={titre}"]
    if Path(args.reference_doc).exists():
        cmd += ["--reference-doc", args.reference_doc]
    subprocess.run(cmd, check=True)

    if not args.md:
        chemin_md.unlink()

    print(f"{len(figs)} figures ({args.figures}) :")
    for lab, cap, *_ in figs:
        print(f"  Figure {numeros[lab]:>2} — {lab:<32} {cap[:58]}")
    print(f"\nÉcrit : {args.out}")


if __name__ == "__main__":
    main()
