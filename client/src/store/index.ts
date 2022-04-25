import Vue from 'vue'
import Vuex from 'vuex'
import { ICharacterOptionsState } from './modules/character-options.module';
Vue.use(Vuex)

export interface IRootState {
  characterOptionsState: ICharacterOptionsState 
}

export default new Vuex.Store<IRootState>({})
