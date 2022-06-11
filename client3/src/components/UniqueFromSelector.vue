<template>
    <SubSelector 
        v-for="(sel, i) in selected"
        :selected="sel"
        :spec="subspec"
        :disabled="false"
        :key="i"
        @choice="selected => updateSelected(selected, i)"
    />
</template>

<script setup lang="ts">
    import { defineProps, defineEmits } from 'vue';
    import SubSelector from './SubSelector.vue';
    import { Selection, Spec, IUniqueFromCharacterOption } from '@/types';
import { validate } from '@babel/types';

    // TODO: define type for this, it's shared with ListSpec etc.
    const props = defineProps<{
        selected: Selection[],
        subspec: Spec,
        num: number
    }>()

    const emit = defineEmits<{
        (e: 'choice', selection: Selection): void
    }>()

    async function updateSelected(sel: Selection, i: number): Promise<void> {
        let newSelected =
            "[" + props.selected.map((val,j) => i === j ? sel : val).join(",") + "]"
        console.log(newSelected)
        emit('choice', newSelected)
    }

    // const prefilled = computed(() =>
    //     props.selectedValue.map()
    // )
</script>