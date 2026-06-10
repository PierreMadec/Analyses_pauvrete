from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter

wb = Workbook()
ws = wb.active
ws.title = "Statut d'occupation"

# Années (une sur deux: 2005 à 2023)
annees = [2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023]

# Catégories de statut d'occupation
statuts = ["Propriétaire", "Locataire non-HLM", "Locataire HLM", "Logé gratuitement"]

# Déciles
deciles = [f"D{i}" for i in range(1, 11)]

# === STRUCTURE DE BASE ===
# Ligne 1: Titre
ws['A1'] = "Répartition des statuts d'occupation par décile de niveau de vie"
ws['A1'].font = Font(bold=True, size=14, color='FFFFFF')
ws['A1'].fill = PatternFill(start_color='1F4E78', end_color='1F4E78', fill_type='solid')
ws.merge_cells('A1:BF1')
ws['A1'].alignment = Alignment(horizontal='center', vertical='center')

# Ligne 2: Année de référence
ws['A2'] = "Source: INSEE - Enquête Revenus Fiscaux et Sociaux (ERFS)"
ws['A2'].font = Font(italic=True, size=10)
ws.merge_cells('A2:BF2')

# Ligne 4: En-têtes - Années
ws['A4'] = "Décile"
ws['A4'].font = Font(bold=True, color='FFFFFF')
ws['A4'].fill = PatternFill(start_color='4472C4', end_color='4472C4', fill_type='solid')

col_year = 2  # Colonne B
for annee in annees:
    col_letter = get_column_letter(col_year)
    ws[f'{col_letter}4'] = annee
    ws[f'{col_letter}4'].font = Font(bold=True, color='FFFFFF')
    ws[f'{col_letter}4'].fill = PatternFill(start_color='4472C4', end_color='4472C4', fill_type='solid')
    ws[f'{col_letter}4'].alignment = Alignment(horizontal='center')
    col_year += 1

# === BLOC PAR STATUT D'OCCUPATION ===
current_row = 5
colors = {
    "Propriétaire": "E7E6E6",
    "Locataire non-HLM": "D9D9D9",
    "Locataire HLM": "CCCCCC",
    "Logé gratuitement": "BFB0CC"
}

for statut in statuts:
    # En-tête du statut
    ws[f'A{current_row}'] = statut
    ws[f'A{current_row}'].font = Font(bold=True, color='FFFFFF', size=11)
    ws[f'A{current_row}'].fill = PatternFill(start_color='5B9BD5', end_color='5B9BD5', fill_type='solid')
    ws.merge_cells(f'A{current_row}:BF{current_row}')
    current_row += 1

    # Données pour chaque décile
    for decile in deciles:
        ws[f'A{current_row}'] = decile
        ws[f'A{current_row}'].font = Font(bold=True)
        ws[f'A{current_row}'].fill = PatternFill(start_color=colors[statut], end_color=colors[statut], fill_type='solid')

        # Ajouter des cellules vides pour chaque année (à remplir)
        col_data = 2
        for _ in annees:
            col_letter = get_column_letter(col_data)
            ws[f'{col_letter}{current_row}'] = ""
            ws[f'{col_letter}{current_row}'].fill = PatternFill(start_color=colors[statut], end_color=colors[statut], fill_type='solid')
            ws[f'{col_letter}{current_row}'].alignment = Alignment(horizontal='center')
            col_data += 1

        current_row += 1

    # Ligne de total pour le statut
    ws[f'A{current_row}'] = f"Total {statut}"
    ws[f'A{current_row}'].font = Font(bold=True, italic=True)
    ws[f'A{current_row}'].fill = PatternFill(start_color=colors[statut], end_color=colors[statut], fill_type='solid')

    col_data = 2
    for col_idx in range(len(annees)):
        col_letter = get_column_letter(col_data)
        # Formule pour sommer les déciles du statut actuel
        start_row = current_row - len(deciles)
        end_row = current_row - 1
        ws[f'{col_letter}{current_row}'] = f'=SUM({col_letter}{start_row}:{col_letter}{end_row})'
        ws[f'{col_letter}{current_row}'].font = Font(bold=True, italic=True)
        ws[f'{col_letter}{current_row}'].fill = PatternFill(start_color=colors[statut], end_color=colors[statut], fill_type='solid')
        ws[f'{col_letter}{current_row}'].number_format = '0.0%'
        col_data += 1

    current_row += 2

# === FEUILLE DE SYNTHÈSE ===
ws_synthese = wb.create_sheet("Synthèse")
ws_synthese['A1'] = "Synthèse des données"
ws_synthese['A1'].font = Font(bold=True, size=12)

ws_synthese['A3'] = "Années couvertes:"
ws_synthese['B3'] = f"{annees[0]} - {annees[-1]}"

ws_synthese['A4'] = "Nombre d'années:"
ws_synthese['B4'] = len(annees)

ws_synthese['A5'] = "Statuts d'occupation:"
ws_synthese['B5'] = len(statuts)

ws_synthese['A6'] = "Déciles:"
ws_synthese['B6'] = len(deciles)

ws_synthese['A8'] = "Instructions pour remplir le fichier:"
ws_synthese['A9'] = "1. Remplir les cellules vides (colonne par année, ligne par décile)"
ws_synthese['A10'] = "2. Les valeurs doivent être exprimées en pourcentage (0.XX)"
ws_synthese['A11'] = "3. La somme des 4 statuts doit égaler 100% pour chaque décile et année"
ws_synthese['A12'] = "4. Les totaux se calculent automatiquement"

# Formatage des colonnes
ws.column_dimensions['A'].width = 20
for col in range(2, len(annees) + 2):
    ws.column_dimensions[get_column_letter(col)].width = 12

# Définir hauteur des lignes
for row in range(1, 4):
    ws.row_dimensions[row].height = 25

# Ajuster hauteur des en-têtes
ws.row_dimensions[4].height = 20

wb.save('statut_occupation_par_decile.xlsx')
print("✓ Fichier créé: statut_occupation_par_decile.xlsx")
print(f"  - {len(annees)} années (2005-2023, une sur deux)")
print(f"  - {len(deciles)} déciles")
print(f"  - {len(statuts)} catégories de statut d'occupation")
