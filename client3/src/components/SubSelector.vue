<template>
    <ListSpec
        v-if="spec.spectype === 'list'"
        :disabled="disabled"
        :selected="(selected as string | null)"
        :options="spec.list"
        @choice="selection => $emit('choice', selection)"
    />
</template>

<script setup lang="ts">
    import { ICharacterOption, Spec } from '@/types';
    import { defineEmits, defineProps } from 'vue';
    import ListSpec from './ListSpec.vue';

    const props = defineProps<{
        spec: Spec,
        selected: Selection | null,
        disabled: boolean
    }>()

    const emit = defineEmits<{
        // choice: Selection | null
        (e:'choice', selection:Selection): void
    }>()

</script>