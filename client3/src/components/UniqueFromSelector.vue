<template>
    <SubSelector/>
</template>

<script setup lang="ts">
    import { defineProps, defineEmits } from 'vue';
    import SubSelector from './SubSelector.vue';
    import { Selection } from '@/types';

    // TODO: define type for this, it's shared with ListSpec etc.
    const props = defineProps<{
        disabled: boolean,
        selectedValue: Selection[],
        options: string[]
    }>()

    
</script>