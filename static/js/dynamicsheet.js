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
        var row = document.createElement('tr');
        ["name","range","to_hit_or_dc","damage","notes"]
            .forEach(function (key) {
                var entry = document.createElement('td');
                entry.innerHTML = attack[key];
                row.appendChild(entry);
            });
        attackTable.appendChild(row);
    })

    // Spellcasting.
    if (sheetData.spell_slots.length > 0) {
        initSpellcasting(sheetData)
    }
}

function initSpellcasting(sheetData) {
    // Header.
    var header = document.createElement("h2");
    header.innerHTML = "Spellcasting";
    document.getElementById("spellcasting")
        .prepend(header);

    // Spell slot table.
    var spellSlotTable = document.createElement("table");
    var spellSlotTableHeaderRow = document.createElement("tr");
    var spellSlotTableSlotRow = document.createElement("tr");

    // Add pact slots if you have them.
    if (sheetData.pact_magic != null) {
        var th = document.createElement("th");
        th.innerHTML
            = `pact magic (level ${sheetData.pact_magic.slot_level})`;
        spellSlotTableHeaderRow.appendChild(th);

        var td = document.createElement("td");
        spellSlotCheckBoxes(td, sheetData.pact_magic.slot_count);
        spellSlotTableSlotRow.appendChild(td);
    }

    sheetData.spell_slots.forEach(function (pair) {
        const level  = pair[0];
        const nSlots = pair[1];

        var th = document.createElement("th");
        th.innerHTML = "lvl " + level;
        spellSlotTableHeaderRow.appendChild(th);

        var td = document.createElement("td");
        spellSlotCheckBoxes(td, nSlots);
        spellSlotTableSlotRow.appendChild(td);
    });
    spellSlotTable.appendChild(spellSlotTableHeaderRow);
    spellSlotTable.appendChild(spellSlotTableSlotRow);
    document.getElementById("spellslots").appendChild(spellSlotTable);
}

function spellSlotCheckBoxes(parent, nSlots) {
    for (var i = 0; i < nSlots; i++) {
        input = document.createElement("input");
        input.setAttribute("type", "checkbox");
        parent.appendChild(input);
    }
}

initPage();
