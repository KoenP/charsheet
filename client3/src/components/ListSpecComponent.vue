<template>
    hello
    <!--select :disabled="disabled"
            @change="event => $emit('choice', event.target.value)">
        <option v-if="selectedValue === undefined" disabled selected value> -- select an option -- </option>
        <option v-for="option in options" :key="option" :selected="option === selectedValue">
            {{option}}
        </option>
    </select-->
</template>

<script setup lang="ts">
    import { Ref, computed, ref, defineProps, defineEmits } from 'vue'
    import { IChoice, ICharacterOption } from '@/types';


    const props = defineProps<{
        disabled: boolean,
        selectedValue?: string,
        options: string[]
    }>()

    const emits = defineEmits<{
        choice: string
    }>()
</script>