import { CharacterOption } from '@/contracts/character-option';
import axios from 'axios';
import qs from 'qs';

const apiClient = axios.create({
  paramsSerializer: qs.stringify,
});

class ApiClient {
  public async getPossibleCharacterOptions(): Promise<Array<CharacterOption>> {
    return (await apiClient.get('request/options')).data;
  }
}

export const api = new ApiClient();