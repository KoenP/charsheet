<template>
  <div>
    <example-drop-down v-for="charOption in characterListOptions"
                       :disabled="loading"
                       :key="`${charOption.id}-${charOption.origin}`"
                       :options="charOption.spec.list"
                       :title="charOption.origin"
                       @input="selectedChoice => updateModel(selectedChoice, charOption)" />
  </div>
</template>

<script lang="ts">
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
  public loading = false;

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
    this.loading = true;
    try {
      await characterOptionModule.updateCharacterOptions(
        fromCharacterOptionToChoice(characterOption, selectedChoice)
      );
    } catch (ex) {
      console.error(ex);
    } finally {
      this.loading = false;
    }
  }
}
</script>

<style lang="scss">
</style>