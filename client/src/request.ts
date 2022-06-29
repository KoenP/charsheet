import axios from 'axios';
import qs from 'qs';
import { ICharacterOption, IChoice, ISheetData, AbilityTableData, Ability } from './types';

const apiClient = axios.create({
  paramsSerializer: qs.stringify,
});

class ApiClient {
  public async getCurrentCharName(): Promise<string> {
    return (await apiClient.get('request/name')).data
  }

  public async getPossibleCharacterOptions(): Promise<ICharacterOption[]> {
    return (await apiClient.get('request/options')).data;
  }

  public async getAbilityTable(): Promise<AbilityTableData> {
    return (await apiClient.get('request/ability_table')).data;
  }

  public async listCharacters(): Promise<string[]> {
    return (await apiClient.get('request/list_characters')).data.list
  }

  public async listClassOptions(): Promise<string[]> {
    return (await apiClient.get('request/list_class_options')).data
  }

  public async getCurLevel(): Promise<number> {
    return (await apiClient.get('request/cur_level')).data
  }

  public async selectSavedCharacter(char: string): Promise<void> {
    return (await apiClient.post('request/load_character', {}, {
      params: {name: char}
    }))
  }

  public async gainLevel(className: string): Promise<void> {
    return (await apiClient.post('request/gain_level', {}, {
      params: {class: className}
    }))
  }

  public async createNewCharacter(name: string): Promise<void> {
    return (await apiClient.post('request/new_character', {}, {
      params: {name: name}
    }))
  }

  public async sheet(): Promise<ISheetData> {
    return (await apiClient.get('request/sheet')).data
  }

  public async registerChoice(choice: IChoice): Promise<void> {
    await apiClient.post('request/choice', {}, {
      params: choice
    })
  }

  public async registerBaseAbilityUpdate(ability: Ability, value: number): Promise<void> {
    await apiClient.post('request/set_base_abilities', {}, {
      params: {[ability]: value}
    })
  }

}

export const api = new ApiClient();