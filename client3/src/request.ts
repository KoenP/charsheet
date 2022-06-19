import axios from 'axios';
import qs from 'qs';
import { ICharacterOption, IChoice, ISheetData } from './types';

const apiClient = axios.create({
  paramsSerializer: qs.stringify,
});

class ApiClient {
  public async getPossibleCharacterOptions(): Promise<ICharacterOption[]> {
    return (await apiClient.get('request/options')).data;
  }

  public async listCharacters(): Promise<string[]> {
    return (await apiClient.get('request/list_characters')).data.list
  }

  public async selectSavedCharacter(char: string): Promise<void> {
    return (await apiClient.post('request/load_character', {}, {
      params: {name: char}
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
}

export const api = new ApiClient();