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

    // Proficiencies.
    document.getElementById("languages").innerHTML
        = "<b>Languages: </b>" + sheetData.languages.join(", ");
    document.getElementById("weapons").innerHTML
        = "<b>Weapons: </b>" + sheetData.weapons.join(", ");
    document.getElementById("armor").innerHTML
        = "<b>Armor: </b>" + sheetData.armor.join(", ");
    document.getElementById("tools").innerHTML
        = "<b>Tools: </b>" + sheetData.tools.join(", ");

    // Attack table.
    var attackTable = document.getElementById("attacks");
    console.log(sheetData);
    sheetData.attacks.forEach(function (attack) {
        console.log(attack);
        var row = document.createElement('tr');
        ["name","range","to_hit_or_dc","damage","notes"]
            .forEach(function (key) {
                var entry = document.createElement('td');
                entry.innerHTML = attack[key];
                row.appendChild(entry);
            });
        attackTable.appendChild(row);
    })
}

initPage();
