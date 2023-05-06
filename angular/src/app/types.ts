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

export type PrologTerm = string | {functor: string, args: PrologTerm[]}

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
