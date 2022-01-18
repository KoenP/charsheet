//put initial values in the summary
async function initSummary() {
    const characterName = await query("name(X)");
    console.log(characterName);
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
initSummary();


