export interface ICharacterOption {
  charlevel: number;
  choice: Selection | null; // TODO: naming is horrible :(
  id: string;
  origin: string;
  origin_category: string;
  spec: Spec
}

export type Selection = (string | number | Selection[])

export interface IListCharacterOption extends ICharacterOption{
  spec: ListSpec;
}

export interface IUniqueFromCharacterOption extends ICharacterOption {
  spec: UniqueFromSpec;
}

export type Spec = (UniqueFromSpec | ListSpec);

export type SpecType = 'list' | 'unique_from'

export type SpecBase = {
  spectype: SpecType;
}

export interface UniqueFromSpec extends SpecBase {
  num: number;
  spec: Spec;
  spectype: 'unique_from';
}

export interface ListSpec extends SpecBase {
  list: string[];
  spectype: 'list';
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