import { ICharacterOption } from '@/contracts/character-option';
import axios from 'axios';
import qs from 'qs';
import { IChoice } from '../contracts/choice';

const apiClient = axios.create({
  paramsSerializer: qs.stringify,
});

class ApiClient {
  public async getPossibleCharacterOptions(): Promise<Array<ICharacterOption>> {
    return (await apiClient.get('request/options')).data;
  }

  public async registerChoice(choice: IChoice): Promise<void> {
    await apiClient.post('request/choice', {}, {
      params: choice
    })
  }
}

export const api = new ApiClient();