<template>
    <ListSpec
        v-if="spec.spectype === 'list'"
        :disabled="disabled"
        :selected="(selected as string | null)"
        :options="spec.list"
        :filter="filter"
        @choice="selection => $emit('choice', selection)"
    />
    <UniqueFromSelector
        v-else-if="spec.spectype === 'unique_from'"
        :selected="selected === null ? [] : selected"
        :subspec="spec.spec"
        :num="spec.num"
        :filter="filter"
        @choice="selection => $emit('choice', selection)"
    />
</template>

<script setup lang="ts">
    import { ICharacterOption, Spec, Selection } from '@/types';
    import { defineEmits, defineProps } from 'vue';
    import ListSpec from './ListSpec.vue';
    import UniqueFromSelector from './UniqueFromSelector.vue';

    const props = defineProps<{
        spec: Spec,
        selected: Selection | null,
        disabled: boolean,
        filter?: string[]
    }>()

    const emit = defineEmits<{
        // choice: Selection | null
        (e:'choice', selection:Selection): void
    }>()

</script>