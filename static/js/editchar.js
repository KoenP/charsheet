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

function groupBy(prop, list) {
    var groups = {};
    for (x of list) {
        const index = prop(x);
        if (index in groups) {
            groups[index].push(x);
        }
        else {
            groups[index] = [x];
        }
    }
    return groups;
}

async function initPage() {
    const charName = await getName();

    document.getElementById("chartitle").innerHTML = "Editing " + charName;
    document.getElementById("pagetitle").innerHTML = charName;
    var sidenav = document.getElementById("sidenav");
    var choicesDiv = document.getElementById("choices");
    var updateDataByCharlevel = {};
    var selectedCharlevel = 1;
    function updateMainBody() {
        choicesDiv.innerHTML = "";
        if (selectedCharlevel === 1) {
        }
        const relevantUpdateDataPerCategory
              = groupBy(x => x.origin_category, updateDataByCharlevel[selectedCharlevel]);
        const sortedCategories = Object.keys(relevantUpdateDataPerCategory).sort();
        for (key of sortedCategories) {
            const choices = relevantUpdateDataPerCategory[key];
            var categoryHeader = document.createElement("h3");
            categoryHeader.innerHTML = `From ${key}:`;
            choicesDiv.appendChild(categoryHeader);
            for (choice of choices) {
                choicesDiv.appendChild(optionsHtml(choice, updatePage));
            }
        }
    }
    async function updatePage() {
        // Fetch data.
        updateDataByCharlevel = groupBy(x => x.charlevel, await requestJson("options", {}));
        //console.log(updateData);

        // Update sidenav.
        sidenav.innerHTML = "";
        const levels = Object.keys(updateDataByCharlevel).map(n => parseInt(n)).filter(n => !isNaN(n)).sort((a,b) => a-b);
        for (const level of levels) {
            var button = document.createElement("button");
            button.innerHTML = `Level ${level}`;
            button.setAttribute("type", "submit")
            button.onclick = function(){selectedCharlevel = level; updatePage();};
            if (selectedCharlevel === level) {
                button.setAttribute("class", "selected");
            }
            sidenav.appendChild(button);
        }
        if ("unknown" in updateDataByCharlevel) {
            var link = document.createElement("a");
            link.innerHTML = "unknown";
            link.onclick = function(){selectedCharlevel = "unknown"; updatePage();};
            sidenav.innerHTML += `unknown`;
        }
        updateMainBody();

        // const abilityTableVals = await requestJson("ability_table", {});
        // if(updateData[index].id == "asi or feat") {
        //     var numberOfAsis = updateData[index].spec.asis;
        //     feats = updateData[index].spec.feats;
        //     drawAsiFeatSelector(abilityTableVals, feats, numberOfAsis);
        // }
        // else {
        //     var limit = updateData[index].spec.num;
        //     options = updateData[index].spec.options;
        //     console.log("ud = " + updateData.map(ud => JSON.stringify(ud)));
        //     console.log("options = " + options);
        //     drawSkillSelector(options, limit);
        // }
    }
    updatePage();
}

initPage();

//todo implement save char
//increases index when action performed, when all actions performed print message to html
//todo loop only if updateData changes
//todo add output window to bottom of html to view all character info?
// async function saveChar() {
// //    await request("save_character", {});
//     var updateData = await receiveUpdate();
//     if(updateIndex < updateData.length) {
//         await updatePage(updateIndex, updateData);
//         updateIndex++;
//     }
//     else {
//         document.getElementById("editmsg").innerHTML = "Done with character TODO for now!"; // todo only show when no update received
//         updateIndex = 0;
//         await updatePage(updateIndex, updateData)
//     }
// }

//point to the correct html draw function (editing asi/feat or skill)

function optionsHtml(optionsData, updateEditPage) {
    const id = optionsData.id;
    const origin = optionsData.origin;
    const spec = optionsData.spec;
    // TODO: choice, if it exists
    var div = document.createElement("div");
    div.innerHTML += `<h5>Choose ${id}</h5>`;
    // div.appendChild(specToHtml(spec).html);

    function registerChoiceAndUpdate(choice) {
        console.log("registerChoiceAndUpdate");
        //console.log({source: origin.toString(), id: id.toString(), choice: choice.toString()});
        request("choice", {source: origin, id: id, choice: choice});
        updateEditPage();
    }

    div.appendChild(selector(spec, registerChoiceAndUpdate, current=optionsData.choice).html);
    return div;
}

var radioIndex = 0;

function selector(spec, onchange, current=null, disabled=false) {
    // List case.
    if (spec.spectype === "list") {
        var dropdown = document.createElement("select");
        var choice = current;
        dropdown.onchange = function() {
            console.log("dropdown.onchange");
            choice = dropdown.value;
            onchange(choice);
        };
        for (elem of spec.list) {
            if (!spec.filter || !spec.filter.includes(elem) || elem === current) {
                var option = document.createElement("option");
                option.value = elem;
                option.text = elem;
                dropdown.appendChild(option);
            }
        }
        if (current === null) {
            const prompt = "<option disabled selected value> -- select an option -- </option>";
            dropdown.innerHTML = prompt + dropdown.innerHTML;
        } else {
            // if (!spec.list.includes(current)) {
            //     var option = document.createElement("option");
            //     option.value = current;
            //     option.text = current;
            //     dropdown.insertBefore(option, dropdown.firstChild);
            // }
            dropdown.value = current;
        }
        if (disabled) {dropdown.disabled = true;}
        return {html: dropdown, getChoice: () => choice};
    }

    // (Unique/non-unique) from case.
    else if (spec.spectype === "unique_from" || spec.spectype === "from") {
        if (spec.num === 1) {
            return selector(spec.spec, onchange, current, disabled);
        }
        var div = document.createElement("div");
        var selectors = [];
        var subspec = spec.spec;

        var choices = current === null ? [] : current;
        function updateChoices() {
            choices = selectors.map(sel => sel.getChoice()).filter(choice => choice != null);
        }

        function init() {
            subspec.filter = spec.spectype === "unique_from" ? choices : [];
            for (choice of choices) {
                var sel = selector(subspec, refresh, current=choice);
                div.appendChild(sel.html);
                selectors.push(sel);
            }
            if (choices.length < spec.num) {
                var sel = selector(subspec, refresh);
                div.appendChild(sel.html);
                selectors.push(sel);
            }
            for (var i = choices.length + 1; i < spec.num; i++) {
                var sel = selector(subspec, refresh, current=null, disabled=true);
                div.appendChild(sel.html);
                selectors.push(sel);
            }
        }

        function refresh(_) {
            updateChoices();
            onchange("[" + choices.toString() + "]");
        }

        init();
        return {html: div, getChoice: () => choices};
    }
    else if (spec.spectype == "or") {
        var div = document.createElement("div");
        var subdiv = document.createElement("div");
        const radioButtonName = "radio" + radioIndex;
        radioIndex += 1;
        var inputs = [];
        const curCopy = current;

        for (const [side, subspec, name]
             of [["left", spec.left, spec.leftname],
                 ["right", spec.right, spec.rightname]]) {
            var input = document.createElement("input");
            console.log(name);
            input.setAttribute("type", "radio");
            input.setAttribute("name", radioButtonName);
            input.setAttribute("value", name);
            const id = radioButtonName + name;
            input.setAttribute("id", id);
            function selectButton() {
                subdiv.innerHTML = "";
                const subCurrent
                      = (curCopy != null && curCopy.side == side)
                      ? curCopy.choice
                      : null;
                console.log(subCurrent);
                subdiv.appendChild(selector(subspec, onchange, current=subCurrent).html);
            }
            input.onchange = selectButton;
            if (current != null && current.side == side) {
                input.checked = true;
                selectButton();
            }
            var label = document.createElement("label");
            label.setAttribute("for", id);
            label.innerHTML = name;
            div.appendChild(input);
            div.appendChild(label);
        }
        div.appendChild(subdiv);
        return {html: div, getChoice: () => subCurrent};
    }

    return {html: document.createElement("div")};
}

// function specToHtml(spec, filtered=new Set()) {
//     if (spec.spectype == "list") {
//         var dropdown = document.createElement("select");
//         const prompt = "<option disabled selected value> -- select an option -- </option>";
//         const disable = function() {dropdown.disabled = true};
//         const enable = function() {dropdown.disabled = false};
//         const setFilter = function(optionsToFilter) {
//             console.log("foo", optionsToFilter);
//             dropdown.innerHTML = prompt;
//             spec.list.forEach(function(elem) {
//                 if (!filtered.has(elem)) {
//                     var option = document.createElement("option");
//                     option.value = elem;
//                     option.text = elem;
//                     dropdown.appendChild(option);
//                 }
//             });
//         };
//         setFilter(filtered);
//         return {html: dropdown, disable: disable, enable: enable,
//                 setFilter: setFilter};
//     }
//     else if (spec.spectype == "unique_from") {
//         const num = spec.num;
//         var specs = [];
//         var selectedOptions = new Set();
//         const handleChange = i => function(){
//             const newVal = this.value;
//             console.log(newVal);
//             if (i < specs.length - 1) {
//                 specs[i+1].enable();
//             }
//             selectedOptions = new Set(specs
//                 .map(spec => spec.html.value)
//                 .filter(string => string.length > 0));
//             console.log(specs);
//             for (const [index, spec] of specs.entries()) {
//                 if (index != i) {
//                     spec.setFilter(selectedOptions);
//                 }
//             }
// 
//             specs.forEach(spec => spec.setFilter(selectedOptions));
//             console.log(selectedOptions);
//         };
//         for (var i = 0; i < num; i++) {
//             var subspec = specToHtml(spec.spec);
//             subspec.html.onchange = handleChange(i);
//             specs.push(subspec);
//         }
//         // second to last subspec starts greyed out
//         for (var i = 1; i < num; i++) {
//             specs[i].disable();
//         }
//         var html = document.createElement("div");
//         specs.forEach(function(subspec) {
//             html.appendChild(subspec.html);
//         });
//         return {html: html}
//     }
//     // else if (spec.spectype == "or") {
//     //     
//     // }
//     else {
//         var p = document.createElement("p");
//         p.innerHTML = "TODO";
//         return {html: p};
//     }
// }

//static for now
async function receiveUpdate() {
    return await requestJson("options", {});
    // var updateData =
    //     [
    //         {
    //         "origin": "Rogue",
    //         "id": "asi or feat",
    //         "spec": {
    //             "spectype": "asi_or_feat",
    //             "asis": "2",
    //             "feats": ["alert", "durable", "war caster"]
    //         },
    //         "choice": []
    //     },
    //     {
    //         "origin": "Rogue",
    //         "id": "skill",
    //         "spec": {
    //             "spectype": "list",
    //             "num": "1",
    //             "options": ["acrobatics", "athletics", "deception", "sleight of hand", "stealth", "persuasion"]
    //         },
    //         "choice": []
    //     },
    //     {
    //         "origin": "rogue",
    //         "id": "skill",
    //         "spec": {
    //             "spectype": "list",
    //             "num": "4",
    //             "options": ["acrobatics", "athletics", "deception", "sleight of hand", "stealth", "persuasion"]
    //         },
    //         "choice": []
    //     }
    //     ]
    // return updateData;
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
            document.getElementById("numberOfAsis").innerHTML = "Ability points: " + numberOfAsis;
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
    var featHTML = document.getElementById("feat");
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
    console.log(radioButtonsFeat);
    document.getElementById("selectBtnFeat").addEventListener("click", () => {
        var selectedSomething = false;
        radioButtonsFeat.forEach(function(radioButton) {
            if(radioButton.checked) {
                console.log(radioButton.value);
                selectedSomething = true;
                saveChar();
            }
        })
        if(!selectedSomething) alert("Please select a feat.");
    });
}

//put skill selector elements in html page
//add necessary vars to selectBtnSkill for use in onclick function
//if select button clicked move on to btnSkillClicked
// function drawSkillSelector(options, limit) {
    //var skillHTML = document.getElementById("skill");
    //if(skillHTML.innerHTML.length != 0) skillHTML.innerHTML = "";
    //
    //var word
    //if(limit > 1) word = "choices";
    //else word = "choice";
    //skillHTML.innerHTML += '<div id="num"> You have ' + limit + ' ' + word + '</div>';
    //options.forEach(function(skill, indexOfSkill) {
    //    skillHTML.innerHTML += 
    //    '<div><input class="skillcheck" type="checkbox" id="skill' + indexOfSkill + '" name="skill" value="' + skill + '">' +
    //    '<label for="skill' + indexOfSkill + '">' + skill + '</label></div>';
    //});
    //skillHTML.innerHTML += "<button id=\"selectBtnSkill\">Select</button>"
    //var selectBtnSkill = document.getElementById("selectBtnSkill");
    //selectBtnSkill.limit = limit;
    //selectBtnSkill.skillHTML = skillHTML;
    //selectBtnSkill.addEventListener("click", btnSkillClicked);
// }

// function btnSkillClicked() {
//     var selectBtnSkill = document.getElementById("selectBtnSkill");
//     let checkBoxes = document.querySelectorAll('input[name="skill"]');
//     let selectedSkills = [];
//     checkBoxes.forEach(function(checkBox) {
//         if(checkBox.checked) {
//             selectedSkills.push(checkBox.value);
//         }
//         
//     });
//     if(selectedSkills.length > selectBtnSkill.limit) {
//         alert("Too many skills selected");
//         checkBoxes.forEach(function(checkBox) {
//             if(checkBox.checked) checkBox.checked = false;
//         });
//     }
//     else if (selectedSkills.length < selectBtnSkill.limit) {
//         alert("Not enough skills selected");
//         checkBoxes.forEach(function(checkBox) {
//             if(checkBox.checked) checkBox.checked = false;
//         });
//     }
//     else {
//         selectedSkills.forEach(function(selectedSkill) {
//             console.log(selectedSkill);
//             selectBtnSkill.skillHTML.innerHTML = "";
//             saveChar();
//         });
//     } 
// }

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
