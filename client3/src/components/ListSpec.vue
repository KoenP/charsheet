<template>
    <select :disabled="disabled"
            @change="event => $emit('choice', event.target.value)">
        <option v-if="selected === null" disabled selected value> -- select an option -- </option>
        <option v-for="option in options" :key="option" :selected="option === selected">
            {{option}}
        </option>
    </select>
</template>

<script setup lang="ts">
    // TODO: rename ListSpec -> ...
    import { Ref, computed, ref, defineProps, defineEmits } from 'vue'
    import { IChoice, ICharacterOption } from '@/types';

    const props = defineProps<{
        disabled: boolean,
        selected: string | null,
        options: string[]
    }>()

    const emits = defineEmits<{
        (e:'choice', selection:Selection): void
    }>()
</script>