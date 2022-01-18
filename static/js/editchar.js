async function initPage() {
    const charName = await getName();
    const abilityTableVals = await requestJson("ability_table", {});

    document.getElementById("chartitle").innerHTML = "Editing " + charName;
    document.getElementById("pagetitle").innerHTML = charName;

    let abilityTable = document.getElementById("abilitytable");
    Array.from(abilityTable.getElementsByClassName("abilityrow"))
        .forEach(function (row) {
            let inputField     = row.getElementsByTagName("input")[0];
            inputField.value   = abilityTableVals.base[row.id];
            inputField.oninput = updateBaseAttribute(row.id);
            row.getElementsByClassName("afterbonuses")[0].innerHTML
                = abilityTableVals.after_bonuses[row.id];
            row.getElementsByClassName("mod")[0].innerHTML
                = abilityTableVals.mods[row.id];
        }
    );
}

initPage();

function updateBaseAttribute(attr) {
    return async function (e) {
        let row = document.getElementById(attr);
        let inputField = row.getElementsByTagName("input")[0];
        await request("set_base_abilities", {[attr]: inputField.value});
        let abilityTableVals = await requestJson("ability_table", {});
        row.getElementsByClassName("afterbonuses")[0].innerHTML
            = abilityTableVals.after_bonuses[attr];
        row.getElementsByClassName("mod")[0].innerHTML
            = abilityTableVals.mods[attr];
    };
}

async function saveChar() {
    await request("save_character", {});
}

