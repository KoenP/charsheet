var updateIndex = 1; //1 because the initial run is done on initPage instead of saveChar

var htmlAbilityTable =
`<p id="numberOfAsis"></p>   
<tr>
   <th>Attribute</th>
   <th>Base value</th>
   <th>After bonuses</th>
   <th>Modifier</th>
 </tr>
 <tr class="abilityrow" id="str">
   <td>str</td>
   <td class="base"></td>
   <td class="afterbonuses"><input type="number"></td>
   <td class="mod"></td>
 </tr>
 <tr class="abilityrow" id="dex">
   <td>dex</td>
   <td class="base"></td>
   <td class="afterbonuses"><input type="number"></td>
   <td class="mod"></td>
 </tr>
 <tr class="abilityrow" id="con">
   <td>con</td>
   <td class="base"></td>
   <td class="afterbonuses"><input type="number"></td>
   <td class="mod"></td>
 </tr>
 <tr class="abilityrow" id="int">
   <td>int</td>
   <td class="base"></td>
   <td class="afterbonuses"><input type="number"></td>
   <td class="mod"></td>
 </tr>
 <tr class="abilityrow" id="wis">
   <td>wis</td>
   <td class="base"></td>
   <td class="afterbonuses"><input type="number"></td>
   <td class="mod"></td>
 </tr>
 <tr class="abilityrow" id="cha">
   <td>cha</td>
   <td class="base"></td>
   <td class="afterbonuses"><input type="number"></td>
   <td class="mod"></td>
 </tr>`;


async function initPage() {
    const charName = await getName();

    document.getElementById("chartitle").innerHTML = "Editing " + charName;
    document.getElementById("pagetitle").innerHTML = charName;
    var updateData = await receiveUpdate();
    updateData.forEach(function(data) {updatePage(data)});
    await updatePage(0, updateData);
}

initPage();

//todo implement save char
//increases index when action performed, when all actions performed print message to html
//todo loop only if updateData changes
//todo add output window to bottom of html to view all character info?
async function saveChar() {
//    await request("save_character", {});
    var updateData = await receiveUpdate();
    if(updateIndex < updateData.length) {
        await updatePage(updateIndex, updateData);
        updateIndex++;
    }
    else {
        document.getElementById("editmsg").innerHTML = "Done with character TODO for now!"; // todo only show when no update received
        updateIndex = 0;
        await updatePage(updateIndex, updateData)
    }
}

//point to the correct html draw function (editing asi/feat or skill)
async function updatePage(data) {
    const abilityTableVals = await requestJson("ability_table", {});
    if(data.id == "asi or feat") {
        var numberOfAsis = data.spec.asis;
        var feats = data.spec.feats;
        drawAsiFeat(abilityTableVals, feats, numberOfAsis);
    }
    else {
        var limit = data.spec.num;
        var options = data.spec.options;
        drawSkillSelector(options, limit);
    }
}

//static for now
async function receiveUpdate() {
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

//todo
async function sendUpdate(dataJson) {
    console.log(dataJson);
}

//check if asi or feat and put elements for choice on the html page
//if asi savechar when all asi points spent
//if feat savechar when feat chosen and button clicked
function drawAsiFeat(abilityTableVals, feats, numberOfAsis) {
    document.getElementById("asiorfeat").innerHTML="";
    console.log(numberOfAsis);
    document.getElementById("abilitytable").innerHTML = htmlAbilityTable;
    document.getElementById("numberOfAsis").innerHTML = numberOfAsis;
    let abilityTable = document.getElementById("abilitytable");

    Array.from(abilityTable.getElementsByClassName("abilityrow")).forEach(function (row) {
        let inputField     = row.getElementsByTagName("input")[0];
        inputField.value   = abilityTableVals.after_bonuses[row.id];
        inputField.oninput = () => {
            abilityTableVals.after_bonuses[row.id] = inputField.value;
            numberOfAsis--;
            document.getElementById("numberOfAsis").innerHTML = numberOfAsis;
            if(numberOfAsis == 0) {
                Array.from(abilityTable.getElementsByClassName("abilityrow")).forEach(function (row) {
                    row.getElementsByClassName("afterbonuses")[0].innerHTML = abilityTableVals.after_bonuses[row.id];
                });
                document.getElementById("numberOfAsis").innerHTML = "";
                saveChar();
            }
        }
        row.getElementsByClassName("base")[0].innerHTML
            = abilityTableVals.base[row.id];
        row.getElementsByClassName("mod")[0].innerHTML
            = abilityTableVals.mods[row.id];
    });
    document.getElementById("asiorfeat").innerHTML = "";
    var featHTML = document.getElementById("feat")
    if(featHTML.length != 0) featHTML.innerHTML = "";
    feats.forEach(function(feat, index) {
        if(index == 0) {
            featHTML.innerHTML +=
            "<div><input type=\"radio\" id=\"radioasi\" name=\"feat\" value=\""+ feat +"\" checked>" +
            "<label for\"radioasi\">"+ feat +"</label></div> " +
            "";
        }
        else {
            featHTML.innerHTML +=
            "<div><input type=\"radio\" id=\"radioasi\" name=\"feat\" value=\""+ feat +"\">" +
            "<label for\"radioasi\">"+ feat +"</label></div> " +
            "";
        }
    });
    featHTML.innerHTML += "<button id=\"selectBtnFeat\">Select</button>";
    let radioButtonsFeat = document.querySelectorAll('input[name="feat"]');
    document.getElementById("selectBtnFeat").addEventListener("click", () => {
        radioButtonsFeat.forEach(function(radioButton) {
            if(radioButton.checked) {
                console.log(radioButton.value);
                saveChar();
            }
            else {
                alert("Please select a feat.")
            }
        })
    });
}

//put skill selector elements in html page
//add necessary vars to selectBtnSkill for use in onclick function
//if select button clicked move on to btnSkillClicked
function drawSkillSelector(options, limit) {
    var skillHTML = document.getElementById("skill");
    
    var word
    if(limit > 1) word = "choices";
    else word = "choice";
    skillHTML.innerHTML += '<div id="num"> You have ' + limit + ' ' + word + '</div>';
    options.forEach(function(skill, indexOfSkill) {
        skillHTML.innerHTML += 
        '<div><input class="skillcheck" type="checkbox" id="skill' + indexOfSkill + '" name="skill" value="' + skill + '">' +
        '<label for="skill' + indexOfSkill + '">' + skill + '</label></div>';
    });
    skillHTML.innerHTML += "<button id=\"selectBtnSkill\">Select</button>"
    var selectBtnSkill = document.getElementById("selectBtnSkill");
    selectBtnSkill.limit = limit;
    selectBtnSkill.skillHTML = skillHTML;
    selectBtnSkill.addEventListener("click", btnSkillClicked);
}

//check if number of selected skills matches limit
//if it doesn't match display alert message and reset form
//else savechar
function btnSkillClicked() {
    var selectBtnSkill = document.getElementById("selectBtnSkill");
    let checkBoxes = document.querySelectorAll('input[name="skill"]');
    let selectedSkills = [];
    checkBoxes.forEach(function(checkBox) {
        if(checkBox.checked) {
            selectedSkills.push(checkBox.value);
        }
        
    });
    if(selectedSkills.length > selectBtnSkill.limit) {
        alert("Too many skills selected");
        checkBoxes.forEach(function(checkBox) {
            if(checkBox.checked) checkBox.checked = false;
        });
    }
    else if (selectedSkills.length < selectBtnSkill.limit) {
        alert("Not enough skills selected");
        checkBoxes.forEach(function(checkBox) {
            if(checkBox.checked) checkBox.checked = false;
        });
    }
    else {
        selectedSkills.forEach(function(selectedSkill) {
            console.log(selectedSkill);
            saveChar();
        });
    } 
}

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