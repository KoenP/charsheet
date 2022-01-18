async function initPage() {
    const charName = await getName();
    const abis = await requestJson("base_abilities", {});
    document.getElementById("chartitle").innerHTML = "Editing " + charName;
    document.getElementById("pagetitle").innerHTML = charName;
    document.getElementById("attrStr").value = abis.str;
    document.getElementById("attrStr").oninput = updateBaseAttribute("str");
    document.getElementById("attrDex").value = abis.dex;
    document.getElementById("attrDex").oninput = updateBaseAttribute("dex");
    document.getElementById("attrCon").value = abis.con;
    document.getElementById("attrCon").oninput = updateBaseAttribute("con");
    document.getElementById("attrInt").value = abis.int;
    document.getElementById("attrInt").oninput = updateBaseAttribute("int");
    document.getElementById("attrWis").value = abis.wis;
    document.getElementById("attrWis").oninput = updateBaseAttribute("wis");
    document.getElementById("attrCha").value = abis.cha;
    document.getElementById("attrCha").oninput = updateBaseAttribute("cha");
}

initPage();

function updateBaseAttribute(attr) {
    return async function (e) {
        await request("set_base_abilities", {[attr]: e.data});
    };
}

