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
        await request("set_base_abilities", {[attr]: e.data});
        let abilityTableVals = await requestJson("ability_table", {});
        let row = document.getElementById(attr);
        row.getElementsByClassName("afterbonuses")[0].innerHTML
            = abilityTableVals.after_bonuses[attr];
        row.getElementsByClassName("mod")[0].innerHTML
            = abilityTableVals.mods[attr];
    };
}

