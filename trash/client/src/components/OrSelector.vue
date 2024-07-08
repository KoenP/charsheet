<template>
    <input
        type="radio"
        id="left"
        value="left"
        v-model="picked"
    />
    <label for="left">{{leftname}}</label>
    <input
        type="radio"
        id="right"
        value="right"
        v-model="picked"
    />
    <label for="right">{{rightname}}</label>
    <div>
        <SubSelector
            v-if="picked === 'left'"
            :spec="left"
            :selected="selected?.side === 'left' ? selected.choice : null"
            :disabled="false"
            :filter="[]"
            @choice="sel => $emit('choice', sel)"
        />
        <SubSelector
            v-else-if="picked === 'right'"
            :spec="right"
            :selected="selected?.side === 'right' ? selected.choice : null"
            :disabled="false"
            :filter="[]"
            @choice="sel => $emit('choice', sel)"
        />
    </div>
</template>

<script setup lang="ts">
    import { Spec, Selection, OrSelection } from '@/types';
    import { Ref, ref, defineProps, defineEmits } from 'vue';
    import SubSelector from './SubSelector.vue';

    // TODO: it seems to me like we shouldn't have to do this stuff manually
    // over and over.
    const props = defineProps<{
        leftname: string,
        rightname: string,
        left: Spec,
        right: Spec,
        selected: OrSelection | null
    }>()

    const emit = defineEmits<{
        (e:'choice', selection:Selection): void
    }>()

    const picked: Ref<"left" | "right" | null> = ref(null)
</script>