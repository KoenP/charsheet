<template>
    <select :disabled="disabled"
            @change="event => $emit('choice', event.target.value)">
        <option v-if="selected === null" disabled selected value>
            -- select an option --
        </option>
        <option v-else-if="!options.includes(selected)" disabled selected value>
            -- ERROR: [{{selected}}] not a valid choice --
        </option>
        <option
            v-for="option in filteredOptions"
            :key="option"
            :selected="option === selected">
            {{option}}
        </option>
    </select>
</template>

<script setup lang="ts">
    // TODO: rename ListSpec -> ListSelector
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

    // Sometimes local filters might apply (for instance if this list is part of
    // a UniqueFromSelector).
    const filteredOptions : ComputedRef<string[]> = computed(() =>
        props.options.filter(x =>
            (!props.filter.includes(x)) || x === props.selected))
</script>