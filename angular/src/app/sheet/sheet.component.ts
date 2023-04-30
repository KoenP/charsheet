import { Component, OnInit } from '@angular/core';
import { ApiService } from '../api.service'
import { Observable, of, BehaviorSubject, map } from 'rxjs';

@Component({
  selector: 'app-sheet',
  templateUrl: './sheet.component.html',
  styleUrls: ['./sheet.component.css']
})
export class SheetComponent {
  sheet$: Observable<ISheetData> = this.api.sheet();
  showOnlyPreparedSpells: boolean = true;
  showOnlyPreparedSpellsToggled$: BehaviorSubject<boolean> = new BehaviorSubject(true);

  constructor(private api: ApiService) {}

  formatModifier(mod: number) {
    return mod >= 0 ? '+' + mod : mod
  }

  abilities(): Ability[] {
    return ['str','dex','con','wis','int','cha'];
  }

  rincl(low: number, high: number): number[] {
    return Array.from(Array((high-low)+1)).map((_,i) => i + low);
  }

  getSpellSlotsAtSpellLevel(sheet: ISheetData, level: number) {
    const n = sheet.spell_slots[level - 1];
    return n === undefined ? 0 : n;
  }

  toggleShowOnlyPreparedSpells(val: boolean) {
    this.showOnlyPreparedSpellsToggled$.next(val);
  }

  filteredSpells$(spells: ISpell[]): Observable<ISpell[]> {
    return this.showOnlyPreparedSpellsToggled$.pipe(
      map((onlyPrepared: boolean) => onlyPrepared
        ? spells
        : spells.filter((spell) => spell.prepared))
    );
  }

////////////////////////////////////////////////////////////////////////////////

  formatPrepared(spell: ISpell): string {
    return spell.prepared ? '✓' : '' // "when prepared" case
  }
  
  formatToHitOrDc(spell: ISpell): string {
    const list = this.notNullSingleton(spell.to_hit)
      .map(this.formatModifier)
      .concat(this.notNullSingleton(spell.dc)
      .map(dc => `DC ${dc} (${spell.dc_abi})`));
    return list.join(",");
  }
  
  formatComponents(cpts: PrologTerm[]): string {
    const reallyThis: SheetComponent = this;
    function formatComponent(cpt: PrologTerm): string {
      return typeof cpt === "string"
        ? cpt
        : reallyThis.htmlTooltip(cpt.functor, cpt.args[0] as string)
    }
    return cpts.map(formatComponent).join('')
  }
  
  htmlTooltip(content: string, tooltiptext: string): string {
    return `<div class="tooltip">${content}<span class="tooltiptext">${tooltiptext}</span></div>`
  }
  
  formatSpellDescription(desc: string[]) {
    return desc.map(paragraph => `<p>${paragraph}</p>`).join('');
  }
  
  //<div :class="trait.desc !== null ? 'tooltip' : null">
  //    {{trait.name}}
  //    <span class="tooltiptext" v-if="trait.desc !== null">
                            //        {{trait.desc}}
  //    </span>
  //</div>
  
  notNullSingleton(val: any) {
    return (val !== null) ? [val] : [];
  }
}

export interface ISpellcastingSection {
  origin: string;
  spellcasting_ability: Ability;
  spellcasting_ability_mod: number;
  spell_save_dc: number;
  spell_attack_mod: number;
  max_prepared_spells: number;
  spells: ISpell[];
}

export interface ISpell {
  prepared: boolean,
  level: number,
  name: string,
  description: string,
  casting_time: string,
  range: string,
  components: Array<string | {functor: 'm', args: string[]}>,
  duration: string,
  concentration: string,
  to_hit: number,
  dc: number,
  dc_abi: Ability,
  summary: string,
  ritual: string,
  resources: string[]
}

export interface CharSummary {
  class: string[];
  race: string;
  level: number;
  maxhp: number;
  ac: number;
  initiative: number;
  speed: number;
  hd: string;
  pp: number;
  prof_bon: number;
}

export type Ability = 'str' | 'con' | 'dex' | 'int' | 'wis' | 'cha'
export type AbilityTableData
  = { [key in Ability]: {mod: number, base: number, score: number, st: number} }

export type SkillTableData = { [key: string]: number }

export interface NotableTraitCategory {
  category: string,
  traits: {desc: string, name: string}[],
}

export interface AttackTableEntry {
  name: string;
  range: string;
  to_hit_or_dc: string;
  damage: string;
  notes: string;
}

export interface ICharacterOption {
  charlevel: number;
  choice: Selection | null; // TODO: naming is horrible :(
  id: string;
  origin: string;
  origin_category: string;
  spec: Spec
}

export type Selection
  = ( string
    | number
    | OrSelection
    | Selection[])

// TODO: refactor
export type OrSelection = {choicetype: 'or', side: 'left' | 'right', choice: Selection}

export interface IListCharacterOption extends ICharacterOption{
  spec: ListSpec;
}

export interface IUniqueFromCharacterOption extends ICharacterOption {
  spec: UniqueFromSpec;
}

export type Spec = (UniqueFromSpec | FromSpec | ListSpec | OrSpec);

export type SpecType = 'list' | 'unique_from' | 'from' | 'or'

export type SpecBase = {
  spectype: SpecType;
}

export interface UniqueFromSpec extends SpecBase {
  num: number;
  spec: Spec;
  spectype: 'unique_from';
}

export interface FromSpec extends SpecBase {
  num: number;
  spec: Spec;
  spectype: 'from';
}

export interface ListSpec extends SpecBase {
  list: string[];
  spectype: 'list';
} 

export interface OrSpec extends SpecBase {
  leftname: string;
  rightname: string;
  left: Spec;
  right: Spec;
  spectype: 'or';
} 

export function isListSpec(spec: Spec): boolean{
  return spec.spectype === 'list';
}

export function isUniqueFromSpec(spec: Spec): boolean{
  return spec.spectype === 'unique_from';
}

export interface IChoice {
  source: string;
  id: string;
  choice: Selection;
}

type PrologTerm = string | {functor: string, args: PrologTerm[]}

export interface ISheetData {
  name: string;
  summary: CharSummary;
  ability_table: AbilityTableData;
  skill_table: SkillTableData;
  languages: string[],
  weapons: string[],
  armor: string[],
  tools: string[],
  notable_traits: NotableTraitCategory[];
  attacks: AttackTableEntry[];
  spell_slots: number[];
  spellcasting_sections: ISpellcastingSection[];
}
