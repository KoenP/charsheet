var updateIndex = 1;
var updateLength;
async function initPage() {
    const charName = await getName();

    document.getElementById("chartitle").innerHTML = "Editing " + charName;
    document.getElementById("pagetitle").innerHTML = charName;

    var updateData = await update();
    updateLength = updateData.length;

    await updatePage(0);
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
//    await request("save_character", {});
    if(updateIndex < updateLength) {
        await updatePage(updateIndex);
        updateIndex++;
    }
    else {
        updateIndex = 0;
        await updatePage(updateIndex);
        updateIndex++;
    }
}

async function updatePage(index) {
    var updateData = await update();
    const abilityTableVals = await requestJson("ability_table", {});
    if(updateData[index].id == "asi or feat") {
        document.getElementById("asiorfeat").innerHTML = 
            "<input type=\"radio\" id=\"radioasi\" name=\"asifeat\" value=\"ASI\" checked>" +
            "<label for\"radioasi\">ASI</label> " +
            "<input type=\"radio\" id=\"radiofeat\" name=\"asifeat\" value=\"FEAT\">" +
            "<label for\"radiofeat\">FEAT</label> " + 
            "<button id=\"selectBtn\">Select</button>";
        let radioButtons = document.querySelectorAll('input[name="asifeat"]');
        document.getElementById("selectBtn").addEventListener("click", () => {
            if(radioButtons[0].checked) {
                document.getElementById("asiorfeat").innerHTML = "";
                console.log("Selected ASI");
                document.getElementById("abilitytable").innerHTML =
                    `<tr>
                    <th>Attribute</th>
                    "<th>Base value</th>
                    <th>After bonuses</th>
                    <th>Modifier</th>
                  </tr>
                  <tr class="abilityrow" id="str">
                    <td>str</td>
                    <td><input type="number"></td>
                    <td class="afterbonuses"></td>
                    <td class="mod"></td>
                  </tr>
                  <tr class="abilityrow" id="dex">
                    <td>dex</td>
                    <td><input type="number"></td>
                    <td class="afterbonuses"></td>
                    <td class="mod"></td>
                  </tr>
                  <tr class="abilityrow" id="con">
                    <td>con</td>
                    <td><input type="number"></td>
                    <td class="afterbonuses"></td>
                    <td class="mod"></td>
                  </tr>
                  <tr class="abilityrow" id="int">
                    <td>int</td>
                    <td><input type="number"></td>
                    <td class="afterbonuses"></td>
                    <td class="mod"></td>
                  </tr>
                  <tr class="abilityrow" id="wis">
                    <td>wis</td>
                    <td><input type="number"></td>
                    <td class="afterbonuses"></td>
                    <td class="mod"></td>
                  </tr>
                  <tr class="abilityrow" id="cha">
                    <td>cha</td>
                    <td><input type="number"></td>
                    <td class="afterbonuses"></td>
                    <td class="mod"></td>
                  </tr>`;
                  
                let abilityTable = document.getElementById("abilitytable");

                Array.from(abilityTable.getElementsByClassName("abilityrow")).forEach(function (row) {
                    let inputField     = row.getElementsByTagName("input")[0];
                    inputField.value   = abilityTableVals.base[row.id];
                    inputField.oninput = updateBaseAttribute(row.id);
                    row.getElementsByClassName("afterbonuses")[0].innerHTML
                        = abilityTableVals.after_bonuses[row.id];
                    row.getElementsByClassName("mod")[0].innerHTML
                        = abilityTableVals.mods[row.id];
                });
            }
        else {
            document.getElementById("asiorfeat").innerHTML = "";
            console.log("Selected FEAT");
            if(document.getElementById("feat").length != 0) document.getElementById("feat").innerHTML = "";
            for (let i = 0; i < updateData[index].spec.feats.length; i++) {
                document.getElementById("feat").innerHTML +=
                "<div><input type=\"radio\" id=\"radioasi\" name=\"feat\" value=\""+ updateData[index].spec.feats[i] +"\">" +
                "<label for\"radioasi\">"+ updateData[index].spec.feats[i] +"</label></div> " +
                "";
            };
            document.getElementById("feat").innerHTML += "<button id=\"selectBtnFeat\">Select</button>";
            let radioButtonsFeat = document.querySelectorAll('input[name="feat"]');
            document.getElementById("selectBtnFeat").addEventListener("click", () => {
                radioButtonsFeat.forEach(function(radioButton) {
                    if(radioButton.checked) {
                        console.log(radioButton.value);
                    }
                })
            });
        }
        });

    }
    else {
        console.log("other update");
        if(document.getElementById("skill").innerHTML.length != 0) document.getElementById("skill").innerHTML = "";
        updateData[index].spec.options.forEach(function(skill, indexOfSkill) {
            document.getElementById("skill").innerHTML += 
            '<div><input type="checkbox" id="skill' + indexOfSkill + '" name="skill" value="' + skill + '">' +
            '<label for="skill' + indexOfSkill + '">' + skill + '</label></div>';
        });
        document.getElementById("skill").innerHTML += "<button id=\"selectBtnSkill\">Select</button>"
        document.getElementById("selectBtnSkill").addEventListener("click", () => {
            let checkBoxes = document.querySelectorAll('input[name="skill"]');
            let selectedSkills = [];
            checkBoxes.forEach(function(checkBox) {
                if(checkBox.checked) {
                    selectedSkills.push(checkBox.value);
                }
            })
            selectedSkills.forEach(function(selectedSkill) {
                console.log(selectedSkill);
            });
        });
    }
}

async function update() {
    var updateData =
        [
            {
            "origin": "Rogue",
            "id": "asi or feat",
            "spec": {
                "spectype": "asi_or_feat",
                "asis": "2",
                "feats": ["alert", "durable", "war caster"]
            },
            "choice": []
        },
        {
            "origin": "Rogue",
            "id": "skill",
            "spec": {
                "spectype": "list",
                "num": "1",
                "options": ["acrobatics", "athletics", "deception", "sleight of hand", "stealth", "persuasion"]
            },
            "choice": []
        },
        {
            "origin": "rogue",
            "id": "skill",
            "spec": {
                "spectype": "list",
                "num": "4",
                "options": ["acrobatics", "athletics", "deception", "sleight of hand", "stealth", "persuasion"]
            },
            "choice": []
        }
        ]
    return updateData;
}