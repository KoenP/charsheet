<template>
    <template v-if="sheet !== null">
        <div class="container">
            <header>
                <button @click="editCharacter" style="float:right">edit</button>
                <h1>{{sheet.name}}</h1>
            </header>
            <article>
                <div>
                    <table id="summary" style="padding: 4px">
                    <tr><th>Race      </th><td>{{sheet.summary.race}}</td></tr>
                    <tr><th>Class     </th><td>{{sheet.summary.class}}</td></tr>
                    <tr><th>Level     </th><td>{{sheet.summary.level}}</td></tr>
                    <tr><th>Max HP    </th><td>{{sheet.summary.maxhp}}</td></tr>
                    <tr><th>AC        </th><td>{{sheet.summary.ac}}</td></tr>
                    <tr><th>Initiative</th><td>{{formatModifier(sheet.summary.initiative)}}</td></tr>
                    <tr><th>Speed     </th><td>{{sheet.summary.speed}}</td></tr>
                    <tr><th>HD        </th><td>{{sheet.summary.hd}}</td></tr>
                    <tr><th>PP        </th><td>{{sheet.summary.pp}}</td></tr>
                    <tr><th>Prof Bon  </th><td>{{formatModifier(sheet.summary.prof_bon)}}</td></tr>
                    </table>
                </div>

            <table id="abilities">
                <caption>
                    <h4>Abilities</h4>
                </caption>
                <tr><th></th><th>Score</th><th>Mod</th><th>ST</th></tr>
                <tr v-for="(data,ability) in sheet.ability_table"
                    :key="ability">
                    <th>{{ability}}</th>
                    <td>{{data.score}}</td>
                    <td>{{formatModifier(data.mod)}}</td>
                    <td>{{formatModifier(data.st)}}</td>
                </tr>
            </table>

            <table id="skills">
                <caption> <h4>Skills</h4> </caption>
                <tr><th></th><th>Skill</th><th>Score</th></tr>
                <tr><td rowspan="1"><b>STR</b></td><td>athletics</td><td>{{formatModifier(sheet.skill_table["athletics"])}}</td></tr>
                <tr><td rowspan="3"><b>DEX</b></td><td>acrobatics</td><td>{{formatModifier(sheet.skill_table["acrobatics"])}}</td></tr>
                <tr><td>sleight of hand</td><td>{{formatModifier(sheet.skill_table["sleight of hand"])}}</td></tr>
                <tr><td>stealth</td><td>{{formatModifier(sheet.skill_table["stealth"])}}</td></tr>
                <tr><td rowspan="5"><b>WIS</b></td><td>animal handling</td><td>{{formatModifier(sheet.skill_table["animal handling"])}}</td></tr>
                <tr><td>insight</td><td>{{formatModifier(sheet.skill_table["insight"])}}</td></tr>
                <tr><td>medicine</td><td>{{formatModifier(sheet.skill_table["medicine"])}}</td></tr>
                <tr><td>perception</td><td>{{formatModifier(sheet.skill_table["perception"])}}</td></tr>
                <tr><td>survival</td><td>{{formatModifier(sheet.skill_table["survival"])}}</td></tr>
                <tr><td rowspan="5"><b>INT</b></td><td>arcana</td><td>{{formatModifier(sheet.skill_table["arcana"])}}</td></tr>
                <tr><td>history</td><td>{{formatModifier(sheet.skill_table["history"])}}</td></tr>
                <tr><td>investigation</td><td>{{formatModifier(sheet.skill_table["investigation"])}}</td></tr>
                <tr><td>nature</td><td>{{formatModifier(sheet.skill_table["nature"])}}</td></tr>
                <tr><td>religion</td><td>{{formatModifier(sheet.skill_table["religion"])}}</td></tr>
                <tr><td rowspan="4"><b>CHA</b></td><td>deception</td><td>{{formatModifier(sheet.skill_table["deception"])}}</td></tr>
                <tr><td>intimidation</td><td>{{formatModifier(sheet.skill_table["intimidation"])}}</td></tr>
                <tr><td>performance</td><td>{{formatModifier(sheet.skill_table["performance"])}}</td></tr>
                <tr><td>persuasion</td><td>{{formatModifier(sheet.skill_table["persuasion"])}}</td></tr>
            </table>

            <h2>Proficiencies</h2>
            <div>
                <ul>
                    <li><b>Languages: </b>{{sheet.languages.join(", ")}}</li>
                    <li><b>Weapons: </b>{{sheet.weapons.join(", ")}}</li>
                    <li><b>Armor: </b>{{sheet.armor.join(", ")}}</li>
                    <li><b>Tools: </b>{{sheet.tools.join(", ")}}</li>
                </ul>
            </div>

            <h2>Notable traits</h2>
            <div>
                <template v-for="category in sheet.notable_traits" :key="category">
                    <h4>
                        From {{category.category}}:
                    </h4>
                    <ul>
                        <li v-for="trait in category.traits" :key="trait.name">
                            <div :class="trait.desc !== null ? 'tooltip' : null">
                                {{trait.name}}
                                <span class="tooltiptext" v-if="trait.desc !== null">
                                    {{trait.desc}}
                                </span>
                            </div>
                        </li>
                    </ul>
                </template>
            </div>

            <table>
                <caption><h4>Attacks</h4></caption>
                <tr><th></th><th>Range</th><th>To Hit</th><th>Damage</th><th>Notes</th></tr>
                <tr v-for="attack in sheet.attacks" :key="attack.name">
                    <td>{{attack.name}}</td>
                    <td>{{attack.range}}</td>
                    <td>{{attack.to_hit_or_dc}}</td>
                    <td>{{attack.damage}}</td>
                    <td>{{attack.notes}}</td>
                </tr>
            </table>

            <h2>Spellcasting</h2>
            <table v-if="Object.keys(sheet.spell_slots) > 0 || sheet.pact_magic !== null">
                <caption><h4>Spell slots</h4></caption>
                <tr><th>Level</th><th v-for="lvl in 9" :key="lvl">{{lvl}}</th></tr>
                <tr v-if="Object.keys(sheet.spell_slots) > 0">
                    <th>Spell slots</th>
                    <td v-for="lvl in 9" :key="lvl">
                        <input type="checkbox" v-for="i in spellSlotsForSpellLevel(lvl)" :key="i"/>
                    </td>
                </tr>
                <tr v-if="sheet.pact_magic !== null">
                    <th>Pact slots</th>
                    <td v-for="lvl in 9" :key="lvl">
                        <input type="checkbox" v-for="i in pactSlotsAtSpellLevel(lvl)" :key="i"/>
                    </td>
                </tr>
            </table>

            <div
                v-for="section in sheet.spellcasting_sections"
                :key="section.origin"
            >
                <h3>{{section.origin}} spells</h3>
                <table class="spelltable">
                    <tr>
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
                    </tr>
                    <tr v-for="spell in section.spells" :key="spell.name">
                        <td>
                            <span v-html="formatAvailability(spell)"></span>
                        </td>
                        <td>{{spell.level}}</td>
                        <td>{{spell.name}}</td>
                        <td>{{spell.casting_time}}</td>
                        <td>{{spell.range}}</td>
                        <td>{{spell.components}}</td>
                        <td>{{spell.duration}}</td>
                        <td>{{spell.concentration}}</td>
                        <td>{{formatToHitOrDc(spell)}}</td>
                        <td>{{spell.summary}}</td>
                        <td>{{spell.resources.join(", ")}}</td>
                    </tr>
                </table>
            </div>


            <!-- 
            <p>

            <h2>Spellcasting</h2>

            <table id="spell_slots">
            <caption>

            <h4>Spell slots</h4>

            </caption>
            </table>

            <p>

            <h3 id="classspellslots"></h3>

            <ul id="classspellcasting">

            </ul>

            <table id="spells">
            <caption>

            <h4>Spells</h4>

            </caption>
            <tr><th>Prep'd</th><th>Lvl</th><th>Src</th><th>Spell</th><th>CT</th><th>Rng</th><th>Cpts</th><th>Dur</th><th>Conc</th><th>To Hit/DC</th><th>Effect (summary)</th><th>Res</th></tr>
            </table>

            </p></p>
            -->

            </article>
        </div>
    </template>
</template>

<script setup lang="ts">
    import { ref, Ref, onMounted } from 'vue';
    import { api } from '@/request';
    import { ISpell, ISheetData, CharSummary, Ability, AbilityTableData, SkillTableData, NotableTrait, AttackTableEntry } from '@/types.ts';
    import { formatModifier } from '@/util'

    const sheet: Ref<ISheetData | null> = ref(null)

    function formatAvailability(spell: ISpell): string {
        //return "TODO"
        return spell.availability === 'always'
            ? "✓"
            : '<input type="checkbox">' // "when prepared" case
    }

    function spellSlotsForSpellLevel(lvl: number): number {
        const nullToZero = (x: any) => x === null ? 0 : x
        return nullToZero(sheet.value.spell_slots?.[lvl])
    }

    function pactSlotsAtSpellLevel(lvl: number): number {
        return sheet.value.pact_magic.slot_level === lvl
            ? sheet.value.pact_magic.slot_count 
            : 0
    }

    function formatToHitOrDc(spell: ISpell): string {
        const list = notNullSingleton(spell.to_hit)
            .map(formatModifier)
            .concat(notNullSingleton(spell.dc)
                    .map(dc => `DC ${dc} (${spell.dc_abi})`));
        return list.join(",");
    }

    function notNullSingleton(val: any) {
        return (val !== null) ? [val] : [];
    }

    async function editCharacter(): Promise<void> {
        window.location.href = 'edit'
    }
    
    onMounted(async function () {
        sheet.value = await api.sheet()
    })
</script>

<style>
    div.container {
        width: 100%;
        font-family: "Liberation", sans-serif;
    }
    header {
        padding: 1em;
        color: black;
        background-color: lightgrey;
        clear: left;
        text-align:left;
    }
    article { padding: 1em; }

    table {
        font-family: "Fira Code", sans-serif;
        border-collapse: collapse;
        border: 1px solid lightgrey;
    }
    table td {
        text-align: right;
        padding: 6px;
        border: 1px solid lightgrey;
    }
    table th { 
        text-align: right;
        padding: 8px;
        background-color: lightgrey;
        border: 1px solid lightgrey;
    }
    table#summary {
        float: right;
    }
    table#abilities tr:nth-child(1) {
        background-color: lightgrey;
    }
    table#abilities td {
        table-layout: fixed;
        width: 25%;
    }
    table#skills tr:nth-child(1) {
        background-color: lightgrey;
    }
    table#attacks tr:nth-child(1) {
        background-color: lightgrey;
    }
    table#spells {
        font-family: "Liberation", sans-serif;
    }
    table#spells tr:nth-child(odd) {
        background-color: lightgrey;
    }
    table#spells tr{
        font-size: 12px;
    }
    table#spells th{
        text-align: left;
    }
    table#spells td{
        text-align: left;
        border: none;
    }
    table#spell_slots th {
        background-color: lightgrey;
    }

    /* From https://www.w3schools.com/css/css_tooltip.asp */
    /* Tooltip container */
    .tooltip {
    position: relative;
    display: inline-block;
    border-bottom: 1px dotted black; /* If you want dots under the hoverable text */
    }

    /* Tooltip text */
    .tooltip .tooltiptext {
    visibility: hidden;
    width: 560px;
    background-color: black;
    color: #fff;
    text-align: center;
    padding: 5px 0;
    border-radius: 6px;
    
    /* Position the tooltip text - see examples below! */
    position: absolute;
    z-index: 1;

    top: 0%;
    left: 100%;
    /* margin-left: -300px; /* Use half of the width (120/2 = 60), to center the tooltip */
    }
    .tooltiptext {
        font-size: 12px;
        font-family: "Liberation", sans-serif;
    }

    /* Show the tooltip text when you mouse over the tooltip container */
    .tooltip:hover .tooltiptext {
    visibility: visible;
    }
</style>