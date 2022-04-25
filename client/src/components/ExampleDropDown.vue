<template>
  <label class="example-drop-down">
    <div>
      {{title}}
    </div>
    <select :disabled="disabled"
            @change="event => $emit('input',event.target.value)">
      <option v-for="option in options"
              :key="option">
        {{option}}
      </option>
    </select>
  </label>
</template>

<script lang="ts">
import { Component, Prop, Vue } from "vue-property-decorator";

@Component({})
export default class ExampleDropDown extends Vue {
  @Prop()
  public options!: string[];
  @Prop()
  public title!: string;

  @Prop()
  public disabled!: string;
}
</script>

<style lang="scss">
.example-drop-down {
  display: flex;
  flex-direction: column;
}
</style>