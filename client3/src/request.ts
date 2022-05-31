import axios from 'axios';
import qs from 'qs';
import { ICharacterOption, IChoice } from './types';

const apiClient = axios.create({
  paramsSerializer: qs.stringify,
});

class ApiClient {
  public async getPossibleCharacterOptions(): Promise<ICharacterOption[]> {
    return (await apiClient.get('request/options')).data;
  }

  public async registerChoice(choice: IChoice): Promise<void> {
    await apiClient.post('request/choice', {}, {
      params: choice
    })
  }
}

export const api = new ApiClient();