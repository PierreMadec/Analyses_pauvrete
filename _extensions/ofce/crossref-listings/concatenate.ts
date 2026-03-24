// Adapté de Toby Driscoll
// garde les fichiers initiaux, opère dans .crossrefs

// const { readDir, readFile, writeFile, remove } = Deno;
const exts: string[] = ['fig', 'tbl', 'tip'];

try {
  await Deno.chdir(".crossrefs");
} catch (error) {
  if (!(error instanceof Deno.errors.NotFound)) {
     Deno.exit();
  }
}

const currentDirectory = Deno.cwd();
const pref = '__'

// Find all files matching "*_exm.yml" in the current directory
for (const ext of exts) {
    const matchingFiles = [];
    for await (const entry of Deno.readDir(currentDirectory)) {
        if (entry.isFile && entry.name.endsWith(ext.concat(pref, '.yml'))) {
            matchingFiles.push(entry.name);
        }
    }
    
    if (matchingFiles.length === 0) {
        // console.log('No matching files found.');
        Deno.exit();
    }
    else {
        // console.log(ext)
        // console.log(matchingFiles)
    }
    
    // Concatenate the matching files
    let concatenatedContent = '';
    for (const file of matchingFiles) {
        const content = await Deno.readTextFile(file);
        concatenatedContent += content;
    }
    
    if (concatenatedContent !== '') {
    // Write the concatenated content to the output file
        const outputFile = ext.concat('.yml');
        await Deno.writeTextFile(outputFile, concatenatedContent);
        // console.debug('Concatenated content written to', outputFile);
    }

    // Delete the matching files
//    for (const file of matchingFiles) {
//        Deno.remove(file);
        // console.debug('Deleted', file);
//    }
}