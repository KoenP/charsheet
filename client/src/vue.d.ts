import { IStore } from './contracts/store-types';
declare module "vue/types/vue" {
  interface Vue {
    $store: Store<IStore>;
  }
}