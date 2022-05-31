<template>
  <div>
    <example-drop-down v-for="(charOption, index) in characterListOptions"
                       :key="index"
                       :disabled="applicationIsBlocked"
                       :options="charOption.spec.list"
                       :title="dropDownTitle(charOption)"
                       @input="selectedChoice => updateModel(selectedChoice, charOption)" />
  </div>
</template>

<script lang="ts">
    // <example-drop-down v-for="x in [0]"
    //                    :key="${x}"
    // >

import {
  IListCharacterOption,
  isListCharacterOption,
} from "@/contracts/character-option";
import { characterOptionModule } from "@/store/modules/character-options.module";
import { Component, Vue } from "vue-property-decorator";
import ExampleDropDown from "../components/ExampleDropDown.vue";
import { ICharacterOption } from "../contracts/character-option";
import { fromCharacterOptionToChoice } from "../util/mapper-functions";

@Component({ components: { ExampleDropDown } })
export default class CharacterSelection extends Vue {
  public applicationIsBlocked = false;

  public async created(): Promise<void> {
    await characterOptionModule.initCharacterOptions();
  }

  public get characterListOptions(): Array<IListCharacterOption> {
    return characterOptionModule.currentCharacterOptions
      .filter((characterOption) => isListCharacterOption(characterOption))
      .sort(
        (o1, o2) => o1.charlevel - o2.charlevel
      ) as Array<IListCharacterOption>;
  }

  public async updateModel(
    selectedChoice: string,
    characterOption: ICharacterOption
  ): Promise<void> {
    this.applicationIsBlocked = true;
    try {
      await characterOptionModule.updateCharacterOptions(
        fromCharacterOptionToChoice(characterOption, selectedChoice)
      );
    } catch (ex) {
      console.error(ex);
    } finally {
      this.applicationIsBlocked = false;
    }
  }

  public dropDownTitle(charOption: ICharacterOption): string {
    return charOption.id + " from " + charOption.origin;
  }
}

</script>

<style lang="scss">
</style>