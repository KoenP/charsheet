<template>
    <select :disabled="disabled"
            @change="event => $emit('choice', event.target.value)">
        <option v-if="selected === null" disabled selected value> -- select an option -- </option>
        <option v-for="option in filteredOptions" :key="option" :selected="option === selected">
            {{option}}
        </option>
    </select>
</template>

<script setup lang="ts">
    // TODO: rename ListSpec -> ...
    import { Ref, computed, ref, defineProps, defineEmits, ComputedRef } from 'vue'
    import { IChoice, ICharacterOption } from '@/types';
import OptionSelectorVue from './OptionSelector.vue';

    const props = defineProps<{
        disabled: boolean,
        selected: string | null,
        options: string[],
        filter: string[]
    }>()

    const emits = defineEmits<{
        (e:'choice', selection:Selection): void
    }>()

    const filteredOptions : ComputedRef<string[]> = computed(() =>
        props.options.filter(x =>
            (!props.filter.includes(x)) || x === props.selected))
</script>