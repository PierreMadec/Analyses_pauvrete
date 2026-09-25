"""
build_sofi_second_revenu.py

Grille 2D pour le levier "second revenu" (couples mono-actifs) : le salaire de
l'adulte 1 (le travailleur pauvre observe) est fixe a des valeurs
representatives (deciles observes dans l'ERFS parmi les couples mono-actifs
pauvres 2021-2023), et on fait varier le salaire de l'adulte 2, de 0 au Smic.

Complementaire de build_sofi_curves.py, qui ne fait varier que l'adulte 1
avec l'adulte 2 fixe a 0 -- insuffisant pour ce levier precis, qui fait
entrer un second revenu.

Produit : figure/sofi_second_revenu.csv
  colonnes : annee, config, salaire_adulte1, salaire_adulte2, niveau_vie
"""

import time
import warnings
import csv

warnings.filterwarnings("ignore")
import formulas

BASE_PATH = "/Users/pierremadec/Documents/GitHub/SOFI"
FILES = {
    2021: "Maquette Sofi 2021 30-01-2023.xlsx",
    2022: "Maquette Sofi 2022.xlsx",
    2023: "Maquette Sofi 2023.xlsx",
}

# Deciles observes du salaire mensuel de l'adulte 1 (couples mono-actifs pauvres, ERFS 2021-2023)
SALAIRE1_POINTS = [0, 300, 625, 900, 1178, 1400, 1618, 1900]
SMIC_MENSUEL = {2021: 1231, 2022: 1269, 2023: 1354}
# Meme decalage de ligne que dans build_sofi_curves.py (verifie par ailleurs)
ROWS = {
    2021: dict(niveau_vie=81),
    2022: dict(niveau_vie=82),
    2023: dict(niveau_vie=81),
}
# grille fine pour l'adulte 2 : 0 -> Smic
N_POINTS_ADULTE2 = 40

CONFIGS = {
    "Couple mono-actif sans enfant": [],
    "Couple mono-actif avec enfant(s)": [8, 12],
}


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

    smic = SMIC_MENSUEL[annee]
    sal2_grid = [round(smic * i / N_POINTS_ADULTE2) for i in range(N_POINTS_ADULTE2 + 1)]
    cell_niveau_vie = f"C{ROWS[annee]['niveau_vie']}"

    for config_nom, enfants in CONFIGS.items():
        base_inputs = {k("C3"): "couple", k("C34"): "salarié", k("C35"): "salarié"}
        for i, age in enumerate(enfants):
            base_inputs[k(f"C{4 + i}")] = age
        for sal1 in SALAIRE1_POINTS:
            t1 = time.time()
            n_ok = 0
            for sal2 in sal2_grid:
                inputs = dict(base_inputs)
                inputs[k("C37")] = sal1
                inputs[k("C38")] = sal2
                try:
                    sol = xl_model.calculate(inputs=inputs)
                    niveau_vie = get(sol, cell_niveau_vie)
                    writer.writerow([annee, config_nom, sal1, sal2, niveau_vie])
                    n_ok += 1
                except Exception:
                    writer.writerow([annee, config_nom, sal1, sal2, ""])
            print(f"    {config_nom} sal1={sal1} : {n_ok}/{len(sal2_grid)} points en {time.time() - t1:.0f}s")


def main():
    out_path = "figure/sofi_second_revenu.csv"
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["annee", "config", "salaire_adulte1", "salaire_adulte2", "niveau_vie"])
        for annee, fname in FILES.items():
            run_year(annee, fname, writer)
    print("Termine :", out_path)


if __name__ == "__main__":
    main()
