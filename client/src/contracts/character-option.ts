export interface ICharacterOption {
  charlevel: number;
  choice: string | null;
  id: string;
  origin: string;
  origin_category: string;
  spec: Spec
}

export interface IListCharacterOption extends ICharacterOption{
  spec: ListSpec;
}

export interface IUniqueFromCharacterOption extends ICharacterOption {
  spec: UniqueFromSpec;
}

type Spec = (UniqueFromSpec | ListSpec);

type SpecType = 'list' | 'unique_from'

type SpecBase = {
  spectype: SpecType;
}

interface UniqueFromSpec extends SpecBase {
  num: number;
  spec: Spec;
  spectype: 'unique_from';
}

interface ListSpec extends SpecBase {
  list: string[];
  spectype: 'list';
} 


export function isListCharacterOption(characterOption: ICharacterOption): boolean{
  return characterOption.spec.spectype === 'list';
}

export function isUniqueFromCharacterOption(characterOption: ICharacterOption): boolean{
  return characterOption.spec.spectype === 'unique_from';
}