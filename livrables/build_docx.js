const fs = require("fs");
const {
  Document, Packer, Paragraph, TextRun, ImageRun, ExternalHyperlink,
  HeadingLevel, AlignmentType, LevelFormat, BorderStyle, ShadingType,
} = require("docx");
const { imageSize: sizeOf } = require("image-size");

const blocks = JSON.parse(fs.readFileSync("parsed_blocks.json", "utf-8"));

const FIG_NUM = {
  "fig-emploi-pauvrete": 1, "fig-rel-anc-trav": 2, "fig-tp3-taux-config": 3,
  "fig-tp-diag": 4, "fig-salaire-distrib": 5, "fig-tp-cf": 6,
  "fig-oaxaca-barre": 7, "fig-cohortes-jeunes": 8,
  "fig-soutien-amortisseur": 9, "fig-decompo-transferts-config": 10,
};

const CITATIONS = {
  madec_wp_2025: "Madec, 2025",
  ponthieux_2009: "Ponthieux, 2009",
  pucci_sofi_2022: "Pucci, 2022",
};

const CAPTIONS = {
  "01_emploi_pauvrete": "Figure 1. Taux d'emploi et taux de pauvreté monétaire, 2005-2023. Source : Insee, ERFS.",
  "02_relatif_ancre": "Figure 2. Taux de pauvreté des personnes en emploi selon le seuil retenu, 2005-2023. Source : Insee, ERFS.",
  "03_taux_par_config": "Figure 3. Taux de pauvreté laborieuse selon la configuration du ménage, 2005-2023. Source : Insee, ERFS.",
  "04_diagnostic_causes": "Figure 4. Causes proximales de la pauvreté laborieuse par configuration de ménage, 2021-2023. Source : Insee, ERFS.",
  "05_distribution_salaire_horaire": "Figure 5. Distribution du salaire horaire rapporté au Smic, travailleurs pauvres et non pauvres, 2013-2023. Source : Insee, ERFS.",
  "06_contrefactuels": "Figure 6. Travailleurs pauvres sortis du seuil selon le levier simulé (maquette SoFi), par configuration, 2021-2023. Source : Insee, ERFS ; Pucci M., SoFi (2022), calculs de l'auteur.",
  "07_oaxaca_composition_coefficients": "Figure 7. Décomposition de Oaxaca-Blinder du taux de pauvreté parmi les travailleurs, 2010-2012 / 2021-2023. Source : Insee, ERFS, calculs de l'auteur.",
  "08_cohortes": "Figure 8. Taux de pauvreté laborieuse à 25-34 ans selon la cohorte de naissance, 2005-2023. Source : Insee, ERFS.",
  "09_effet_amortisseur": "Figure 9. Effet amortisseur de l'enveloppe de soutien aux bas revenus d'activité, 2005-2023. Source : Insee, ERFS, calculs de l'auteur.",
  "10_decompo_transferts": "Figure 10. Effet amortisseur de chaque poste de transferts par configuration de ménage, 2021-2023. Source : Insee, ERFS, calculs de l'auteur.",
};

// Nettoie les références de figures et citations, en conservant les liens markdown [text](url)
function preClean(text) {
  text = text.replace(/\(@(fig-[a-z0-9-]+)\)/g, (m, key) => `(figure ${FIG_NUM[key] || "?"})`);
  text = text.replace(/@(fig-[a-z0-9-]+)/g, (m, key) => `figure ${FIG_NUM[key] || "?"}`);
  text = text.replace(/\[@([a-z0-9_]+)\]/g, (m, key) => `(${CITATIONS[key] || key})`);
  return text;
}

// Convertit une chaine avec **gras** et [texte](url) en tableau de TextRun / ExternalHyperlink
function parseInline(text) {
  text = preClean(text);
  const runs = [];
  // Tokenize sur ** et [..](..)
  const re = /(\*\*(.+?)\*\*)|(\[([^\]]+)\]\(([^)]+)\))/g;
  let last = 0, m;
  while ((m = re.exec(text)) !== null) {
    if (m.index > last) runs.push(new TextRun({ text: text.slice(last, m.index), font: "Arial", size: 22 }));
    if (m[1]) {
      runs.push(new TextRun({ text: m[2], bold: true, font: "Arial", size: 22 }));
    } else if (m[3]) {
      runs.push(new ExternalHyperlink({
        link: m[5],
        children: [new TextRun({ text: m[4], style: "Hyperlink", font: "Arial", size: 22 })],
      }));
    }
    last = re.lastIndex;
  }
  if (last < text.length) runs.push(new TextRun({ text: text.slice(last), font: "Arial", size: 22 }));
  return runs;
}

const children = [];

children.push(new Paragraph({
  heading: HeadingLevel.TITLE,
  children: [new TextRun({ text: "Emploi en hausse, pauvreté qui ne recule plus : le travail protège-t-il encore ?", font: "Arial" })],
}));
children.push(new Paragraph({
  children: [new TextRun({ text: "Pierre Madec (OFCE, Sciences Po Paris)", italics: true, font: "Arial", size: 22 })],
  spacing: { after: 300 },
}));

for (const b of blocks) {
  if (b.type === "h1") {
    children.push(new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun({ text: b.text, font: "Arial" })] }));
  } else if (b.type === "h2") {
    children.push(new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun({ text: b.text, font: "Arial" })] }));
  } else if (b.type === "para") {
    children.push(new Paragraph({ children: parseInline(b.text), spacing: { after: 200 }, alignment: AlignmentType.JUSTIFIED }));
  } else if (b.type === "bullets") {
    for (const item of b.items) {
      children.push(new Paragraph({
        numbering: { reference: "biblio-bullets", level: 0 },
        children: parseInline(item),
      }));
    }
  } else if (b.type === "callout") {
    children.push(new Paragraph({
      border: { top: { style: BorderStyle.SINGLE, size: 6, color: "1F4E78", space: 8 } },
      spacing: { before: 200 },
      children: [new TextRun({ text: b.title, bold: true, italics: true, font: "Arial", size: 22, color: "1F4E78" })],
    }));
    children.push(new Paragraph({
      children: parseInline(b.text).map((r) => r),
      spacing: { after: 200 },
      shading: { fill: "F2F6FA", type: ShadingType.CLEAR },
    }));
  } else if (b.type === "image") {
    const imgPath = `images/${b.file}.png`;
    const imgData = fs.readFileSync(imgPath);
    const dims = sizeOf(imgData);
    const maxWidth = 560; // points, ~ page content width
    const ratio = dims.height / dims.width;
    children.push(new Paragraph({
      children: [new ImageRun({
        type: "png",
        data: imgData,
        transformation: { width: maxWidth, height: Math.round(maxWidth * ratio) },
        altText: { title: b.file, description: CAPTIONS[b.file] || b.file, name: b.file },
      })],
      alignment: AlignmentType.CENTER,
      spacing: { before: 100, after: 100 },
    }));
    children.push(new Paragraph({
      children: [new TextRun({ text: CAPTIONS[b.file] || "", italics: true, size: 18, font: "Arial", color: "595959" })],
      alignment: AlignmentType.CENTER,
      spacing: { after: 300 },
    }));
  }
}

const doc = new Document({
  numbering: {
    config: [{
      reference: "biblio-bullets",
      levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT,
        style: { paragraph: { indent: { left: 720, hanging: 360 } } } }],
    }],
  },
  styles: {
    default: { document: { run: { font: "Arial", size: 22 } } },
    paragraphStyles: [
      { id: "Title", name: "Title", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 36, bold: true, font: "Arial" },
        paragraph: { spacing: { after: 120 } } },
      { id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 30, bold: true, font: "Arial" },
        paragraph: { spacing: { before: 300, after: 180 }, outlineLevel: 0 } },
      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 26, bold: true, font: "Arial", color: "1F4E78" },
        paragraph: { spacing: { before: 260, after: 140 }, outlineLevel: 1 } },
    ],
  },
  sections: [{
    properties: {
      page: {
        size: { width: 11906, height: 16838 }, // A4
        margin: { top: 1440, right: 1440, bottom: 1440, left: 1440 },
      },
    },
    children,
  }],
});

Packer.toBuffer(doc).then((buffer) => {
  fs.writeFileSync("insee_référence_article.docx", buffer);
  console.log("Saved insee_référence_article.docx");
});
