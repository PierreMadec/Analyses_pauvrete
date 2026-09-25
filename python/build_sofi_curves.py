"""
build_sofi_curves.py

Balaye le salaire de la maquette SoFi (Pucci, 2022 ; github.com/murielpucci/SOFI)
pour reconstituer, pour chaque configuration de menage de l'article et chaque
annee 2021-2023, une courbe salaire -> niveau de vie qui integre correctement
RSA, prime d'activite, aides au logement et impot sur le revenu -- au lieu du
taux de reprise forfaitaire (38 %) utilise jusqu'ici dans
contrefactuels_travailleurs_pauvres.R.

Age des enfants fixes a des valeurs representatives (milieu d'enfance) faute
de pouvoir varier cette dimension sans exploser le nombre de simulations.

Produit : figure/sofi_curves.csv
  colonnes : annee, config, salaire_mensuel, niveau_vie, rsa, ppa, apl
"""

import time
import warnings
import csv

warnings.filterwarnings("ignore")
import formulas

BASE_PATH = "/Users/pierremadec/Documents/GitHub/SOFI"
FILES = {
    2021: "Maquette Sofi 2021 30-01-2023.xlsx",
    2023: "Maquette Sofi 2023.xlsx",
}
# La ligne des resultats (RSA/PPA/APL/Niveau de vie) decale d'une ligne selon
# la version du fichier -- verifie manuellement pour chaque annee.
ROWS = {
    2021: dict(apl=67, rsa=73, ppa=74, niveau_vie=81),
    2022: dict(apl=68, rsa=74, ppa=75, niveau_vie=82),
    2023: dict(apl=67, rsa=73, ppa=74, niveau_vie=81),
}

# grille de salaires mensuels balayee (euros)
SALAIRE_GRID = list(range(0, 3501, 25))

CONFIGS = {
    "Personne seule": dict(situation="isolé", enfants=[], statut2=None, sal2=0),
    "Famille monoparentale 1 enfant": dict(situation="isolé", enfants=[10], statut2=None, sal2=0),
    "Famille monoparentale 2 enfants": dict(situation="isolé", enfants=[8, 12], statut2=None, sal2=0),
    "Famille monoparentale 3 enfants": dict(situation="isolé", enfants=[6, 10, 14], statut2=None, sal2=0),
    "Couple mono-actif sans enfant": dict(situation="couple", enfants=[], statut2="inactif", sal2=0),
    "Couple mono-actif avec enfant(s)": dict(situation="couple", enfants=[8, 12], statut2="inactif", sal2=0),
}
# Couple bi-actif : le 2e adulte a un salaire fixe (1,6 x Smic, cf. calibrage
# deja mene sur les couples bi-actifs proches du seuil dans l'article).
SMIC_MENSUEL = {2021: 1231, 2022: 1269, 2023: 1354}  # cf. data/parametres_macro.csv (smic_net)


def run_year(annee, fname, writer):
    path = f"{BASE_PATH}/{fname}"
    t0 = time.time()
    xl_model = formulas.ExcelModel().loads(path).finish()
    print(f"  {annee} : modele charge en {time.time() - t0:.0f}s")

    sheet_key = f"[{fname}]CAS TYPE"

    def k(cell):
        return f"'{sheet_key}'!{cell}"

    def get(sol, cell):
        v = sol[k(cell)].value
        try:
            return v[0, 0]
        except Exception:
            return v

    r = ROWS[annee]
    cell_niveau_vie, cell_rsa, cell_ppa, cell_apl = (
        f"C{r['niveau_vie']}", f"C{r['rsa']}", f"C{r['ppa']}", f"C{r['apl']}"
    )

    configs_annee = dict(CONFIGS)
    sal2_biactif = round(1.6 * SMIC_MENSUEL[annee])
    configs_annee["Couple bi-actif avec enfant(s)"] = dict(
        situation="couple", enfants=[8, 12], statut2="salarié", sal2=sal2_biactif
    )

    for config_nom, cfg in configs_annee.items():
        base_inputs = {k("C3"): cfg["situation"]}
        for i, age in enumerate(cfg["enfants"]):
            base_inputs[k(f"C{4 + i}")] = age
        if cfg["situation"] == "couple":
            base_inputs[k("C34")] = "salarié"
            base_inputs[k("C35")] = cfg["statut2"]
            base_inputs[k("C38")] = cfg["sal2"]
        t1 = time.time()
        n_ok = 0
        for sal in SALAIRE_GRID:
            inputs = dict(base_inputs)
            inputs[k("C37")] = sal
            try:
                sol = xl_model.calculate(inputs=inputs)
                niveau_vie = get(sol, cell_niveau_vie)
                rsa = get(sol, cell_rsa)
                ppa = get(sol, cell_ppa)
                apl = get(sol, cell_apl)
                writer.writerow([annee, config_nom, sal, niveau_vie, rsa, ppa, apl])
                n_ok += 1
            except Exception as e:
                writer.writerow([annee, config_nom, sal, "", "", "", ""])
        print(f"    {config_nom} : {n_ok}/{len(SALAIRE_GRID)} points en {time.time() - t1:.0f}s")


def main():
    out_path = "figure/sofi_curves.csv"
    with open(out_path, "a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        for annee, fname in FILES.items():
            run_year(annee, fname, writer)
    print("Termine :", out_path)


if __name__ == "__main__":
    main()
