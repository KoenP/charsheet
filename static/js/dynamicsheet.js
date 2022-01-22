//put initial values in the summary
async function initPage() {
    const sheetData = await requestJson("sheet", {});
    const characterName = sheetData.name;
    document.getElementById("nametitle").innerHTML = characterName;
    document.getElementById("nameheader").innerHTML = characterName;

    // Character summary.
    for (const key in sheetData.summary) {
        document.getElementById(key).innerHTML = sheetData.summary[key];
    }

    // Ability table.
    for (const key in sheetData.ability_table) {
        document.getElementById(key).innerHTML = sheetData.ability_table[key];
    }

    // Skill table.
    for (const key in sheetData.skill_table) {
        document.getElementById(key).innerHTML = sheetData.skill_table[key];
    }
}

initPage();
