<template>
    <SubSelector 
        v-for="(sel, i) in selected"
        :selected="sel"
        :spec="subspec"
        :disabled="false"
        :key="i"
        :filter="subFilter"
        @choice="selected => updateSelected(selected, i)"
    />
    <SubSelector
        v-if="selected.length < num"
        :selected="null"
        :spec="subspec"
        :disabled="false"
        :key="selected.length"
        :filter="subFilter"
        @choice="appendSelected"
    />
    <template v-if="num - selected.length > 0">
        <SubSelector
            v-for="i in (num - selected.length - 1)"
            :selected="null"
            :spec="subspec"
            :disabled="true"
            :key="i + selected.length"
            :filter="subFilter"
        />
    </template>
</template>

<script setup lang="ts">
    import { defineProps, defineEmits, ComputedRef, computed } from 'vue';
    import SubSelector from './SubSelector.vue';
    import { Selection, Spec, IUniqueFromCharacterOption } from '@/types';
import { validate } from '@babel/types';

    // TODO: define type for this, it's shared with ListSpec etc.
    const props = defineProps<{
        selected: Selection[],
        subspec: Spec,
        num: number,
        filter: string[]
    }>()

    const emit = defineEmits<{
        (e: 'choice', selection: Selection): void
    }>()

    const subFilter: ComputedRef<string[]> = computed(function() {
        const newElems: string[] =
            props.selected.filter(x => typeof x === "string") as string[]
        return props.filter.concat(newElems)
    })

    async function updateSelected(sel: Selection, i: number): Promise<void> {
        let newSelected =
            "[" + props.selected.map((val,j) => i === j ? sel : val).join(",") + "]"
        console.log(newSelected)
        emit('choice', newSelected)
    }

    async function appendSelected(sel: Selection) : Promise<void> {
        let newSelected = "[" + props.selected.concat([sel]) + "]"
        console.log(newSelected)
        emit('choice', newSelected)
    }

    // const prefilled = computed(() =>
    //     props.selectedValue.map()
    // )
</script>