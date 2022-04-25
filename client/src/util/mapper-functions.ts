import { ICharacterOption } from '../contracts/character-option';
import { IChoice } from '../contracts/choice';

export function fromCharacterOptionToChoice(characterOption: ICharacterOption, chosenOption:string): IChoice{
  return {
    choice: chosenOption,
    id: characterOption.id,
    source: characterOption.origin
  }
}


