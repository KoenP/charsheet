//put initial values in the summary
async function initSummary() {
    const characterName = await query("name(X)");
    document.getElementById("nametitle").innerHTML = characterName;
    document.getElementById("nameheader").innerHTML = characterName;
    document.getElementById("race").innerHTML = await query("race(X)");
    document.getElementById("class").innerHTML = await query("class(X)");
    document.getElementById("level").innerHTML = await query("level(X)");
    document.getElementById("maxhp").innerHTML = await query("max_hp(X)");
    document.getElementById("ac").innerHTML = await query("ac(X)");
    document.getElementById("initiative").innerHTML = await query("initiative(X)");
    document.getElementById("speed").innerHTML = await query("speed(X)") + " ft";
    document.getElementById("hd").innerHTML = await query("hit_dice(X)");
    document.getElementById("pp").innerHTML = await query("passive_perception(X)");
    document.getElementById("prof_bon").innerHTML = await query("proficiency_bonus(X)");
}

async function initAbilities() {
    document.getElementById("strscore").innerHTML = await query("ability(str,X)");
    document.getElementById("dexscore").innerHTML = await query("ability(dex,X)");
    document.getElementById("conscore").innerHTML = await query("ability(con,X)");
    document.getElementById("intscore").innerHTML = await query("ability(int,X)");
    document.getElementById("wisscore").innerHTML = await query("ability(wis,X)");
    document.getElementById("chascore").innerHTML = await query("ability(cha,X)");
    document.getElementById("strmod").innerHTML = await query("ability_mod(str,X)");
    document.getElementById("dexmod").innerHTML = await query("ability_mod(dex,X)");
    document.getElementById("conmod").innerHTML = await query("ability_mod(con,X)");
    document.getElementById("intmod").innerHTML = await query("ability_mod(int,X)");
    document.getElementById("wismod").innerHTML = await query("ability_mod(wis,X)");
    document.getElementById("chamod").innerHTML = await query("ability_mod(cha,X)");
    document.getElementById("strst").innerHTML = await query("saving_throw(str,X)");
    document.getElementById("dexst").innerHTML = await query("saving_throw(dex,X)");
    document.getElementById("const").innerHTML = await query("saving_throw(con,X)");
    document.getElementById("intst").innerHTML = await query("saving_throw(int,X)");
    document.getElementById("wisst").innerHTML = await query("saving_throw(wis,X)");
    document.getElementById("chast").innerHTML = await query("saving_throw(cha,X)");
}

async function initSkills() {
    document.getElementById("athletics").innerHTML = await query("skill(athletics,X)");
    document.getElementById("acrobatics").innerHTML = await query("skill(acrobatics,X)");
    document.getElementById("sleight").innerHTML = await query("skill('sleight of hand',X)");
    document.getElementById("stealth").innerHTML = await query("skill(stealth,X)");
    document.getElementById("animal").innerHTML = await query("skill('animal handling',X)");
    document.getElementById("insight").innerHTML = await query("skill(insight,X)");
    document.getElementById("medicine").innerHTML = await query("skill(medicine,X)");
    document.getElementById("perception").innerHTML = await query("skill(perception,X)");
    document.getElementById("survival").innerHTML = await query("skill(survival,X)");
    document.getElementById("arcana").innerHTML = await query("skill(arcana,X)");
    document.getElementById("history").innerHTML = await query("skill(history,X)");
    document.getElementById("investigation").innerHTML = await query("skill(investigation,X)");
    document.getElementById("nature").innerHTML = await query("skill(nature,X)");
    document.getElementById("religion").innerHTML = await query("skill(religion,X)");
    document.getElementById("deception").innerHTML = await query("skill(deception,X)");
    document.getElementById("intimidation").innerHTML = await query("skill(intimidation,X)");
    document.getElementById("performance").innerHTML = await query("skill(performance,X)");
    document.getElementById("persuasion").innerHTML = await query("skill(persuasion,X)");
}

async function initPage() {
    await initSummary();
    await initAbilities();
    await initSkills();
}
initPage();