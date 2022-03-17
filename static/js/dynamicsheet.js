//put initial values in the summary
async function initPage() {
    const sheetData = await requestJson("sheet", {});
    console.log(sheetData);
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

    // Notable traits.
    var traitList = document.getElementById("traitlist");
    sheetData.notable_traits.forEach(function (trait) {
        const item = maybeAddTooltip(trait.trait, trait.desc);
        traitList.innerHTML += `<li>${item}</li>`;
    });

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
    if (sheetData.spell_slots.length > 0 || sheetData.pact_magic != null) {
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

    // Add spellcasting sections.
    sheetData.spellcasting_sections.forEach(initSpellcastingSection);
}

function initSpellcastingSection(sectionData) {
    const abiMod = formatBonus(sectionData.spellcasting_ability_mod);
    const attackMod = formatBonus(sectionData.spell_attack_mod);
    const prep = (sectionData.max_prepared_spells != null)
          ? `<li>Max prepared spells: ${sectionData.max_prepared_spells}</li>`
          : "";
    const sectionHTML =
          `<h3>${sectionData.origin}</h3>
           <ul>
             ${prep}
             <li>Spellcasting ability: ${sectionData.spellcasting_ability}(${abiMod})</li>
             <li>Spell save DC: ${sectionData.spell_save_dc}</li>
             <li>Spell attack modifier: ${attackMod}</li>
           </ul>`

    var table = document.createElement("table");
    table.setAttribute("class", "spelltable");
    table.innerHTML =
          `<tr>
             <th>Prep'd</th>
             <th>Lvl</th>
             <th>Spell</th>
             <th>CT</th>
             <th>Rng</th>
             <th>Cpts</th>
             <th>Dur</th>
             <th>Conc</th>
             <th>To Hit/DC</th>
             <th>Effect (summary)</th>
             <th>Res</th>
           </tr>`
    sectionData.spells.forEach(sd => addSpellTableRow(table,sd));

    var spellcastingDiv = document.getElementById("spellcasting");
    spellcastingDiv.innerHTML += sectionHTML;
    spellcastingDiv.appendChild(table);
}

function addSpellTableRow(table, sd) {
    table.innerHTML +=
      `<tr>
         <td>${formatAvailability(sd.availability)}</td>
         <td>${sd.level}</td>
         <td>${formatSpellName(sd)}</td>
         <td>${sd.casting_time}</td>
         <td>${sd.range}</td>
         <td>${sd.components}</td>
         <td>${sd.duration}</td>
         <td>${sd.concentration}</td>
         <td>${formatToHitOrDc(sd)}</td>
         <td>${sd.summary}</td>
         <td>${formatResources(sd.resources)}</td>
       </tr>
      `;
}

function formatAvailability(availability) {
    return {
        "always": "✓",
        "when prepared": `<input type="checkbox">`
    }[availability];
}

function formatBonus(bonus) {
    if (bonus >= 0) {
        return '+' + bonus;
    } else {
        return bonus;
    }
}

function formatResources(resources) {
    return (resources.length == 0 ? "-" : resources.join(", "));
}

function formatSpellName(spellData) {
    return maybeAddTooltip(spellData.name, spellData.description);
    // return `<div class="tooltip">
    //           ${spellData.name}
    //           <span class="tooltiptext">
    //             ${spellData.description}
    //           </span>
    //         </div>`;
}

function formatToHitOrDc(spellData) {
    const list = notNullSingleton(spellData.to_hit)
          .map(formatBonus)
          .concat(notNullSingleton(spellData.dc)
                  .map(dc => `DC ${dc} (${spellData.dc_abi})`));
    return list.join(",");
}

function formatResources(resources) {
    console.log(resources);
    const tag = resources.tag;
    const val = resources.val;
    const formatResList = function (list) {
        console.log("TEST");
        console.log(list);
        return list.map(formatResources).join(", ");
    };
    return {
        "val": val,
        "or": "todo",
        "per_rest": "todo",
        "list": formatResList(val)
    }[tag];
}

function maybeAddTooltip(mainText, tooltipText) {
    if (tooltipText != null)
        return `<div class="tooltip">
                  ${mainText}
                  <span class="tooltiptext">
                    ${tooltipText}
                  </span>
                </div>`
    else return mainText;
}

function notNullSingleton(val) {
    return (val != null) ? [val] : [];
}


function spellSlotCheckBoxes(parent, nSlots) {
    for (var i = 0; i < nSlots; i++) {
        input = document.createElement("input");
        input.setAttribute("type", "checkbox");
        parent.appendChild(input);
    }
}

function addTooltip(text, tooltip) {
    if (tooltip != null) {
        return `<div class="tooltip">
                  ${text}
                  <span class="tooltiptext">
                    ${tooltip}
                  </span>
                </div>`;
    } else {
        return text;
    }

}

initPage();
