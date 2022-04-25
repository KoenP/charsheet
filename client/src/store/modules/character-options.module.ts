import { ICharacterOption } from "@/contracts/character-option";
import { Action, getModule, Module, Mutation, VuexModule } from "vuex-module-decorators";
import store from "@/store"
import { api } from '../../services/api-client';
import { IChoice } from '../../contracts/choice';

export interface ICharacterOptionsState{
  currentCharacterOptions: Array<ICharacterOption>;
  isInitialized: boolean;
}

@Module({ dynamic: true, store, name: 'characterOptions' })
class CharacterOptionModule extends VuexModule implements ICharacterOptionsState{
  currentCharacterOptions: ICharacterOption[] = [];
  isInitialized = false;

  @Mutation
  public SET_CHARACTER_OPTIONS(options: Array<ICharacterOption>) {
    this.currentCharacterOptions = options;
  }

  @Mutation
  public SET_STORE_INITIALIZED() {
    this.isInitialized = true;
  }

  @Action({})
  public async updateCharacterOptions(choice: IChoice): Promise<void>{
    await api.registerChoice(choice);
    const newOptions = await api.getPossibleCharacterOptions();
    this.SET_CHARACTER_OPTIONS(newOptions);
  }

  @Action({})
  public async initCharacterOptions(): Promise<void>{
    if (this.isInitialized) {
      return;
    }
    const newOptions = await api.getPossibleCharacterOptions();
    this.SET_CHARACTER_OPTIONS(newOptions);
    this.SET_STORE_INITIALIZED()
  }
}

export const characterOptionModule = getModule(CharacterOptionModule);