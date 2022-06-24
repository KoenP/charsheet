<template>
    <!-- Selectors for which a choice has already been made. -->
    <SubSelector 
        v-for="(sel, i) in selected"
        :selected="sel"
        :spec="subspec"
        :disabled="false"
        :key="i"
        :filter="subFilter"
        @choice="selected => updateSelected(selected, i)"
    />

    <!-- 
        If there are less than num choices already selected,
        we add a selector to add a new choice.
    -->
    <SubSelector
        v-if="selected.length < num"
        :selected="null"
        :spec="subspec"
        :disabled="false"
        :key="selected.length"
        :filter="subFilter"
        @choice="appendSelected"
    />

    <!-- Disabled selectors for unmade choices.  -->
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
        filter: string[],
        unique: boolean
    }>()

    const emit = defineEmits<{
        (e: 'choice', selection: Selection): void
    }>()

    // If this is a unique_from spec and a choice has been selected in one
    // of this component's subselectors, it shouldn't be offered in the other
    // subselectors.
    const subFilter: ComputedRef<string[]> = computed(function() {
        if (props.unique) { 
            const newElems: string[] =
                props.selected.filter(x => typeof x === "string") as string[]
            return props.filter.concat(newElems)
        }
        else {
            return props.filter
        }
    })

    // Update a previously made choice.
    async function updateSelected(sel: Selection, i: number): Promise<void> {
        // TODO!! this is a horror show
        let newSelected =
            "[" + props.selected.map((val,j) => i === j ? sel : val).join(",") + "]"
        emit('choice', newSelected)
    }

    // Add a new choice.
    async function appendSelected(sel: Selection) : Promise<void> {
        let newSelected = "[" + props.selected.concat([sel]) + "]"
        emit('choice', newSelected)
    }

    // const prefilled = computed(() =>
    //     props.selectedValue.map()
    // )
</script>