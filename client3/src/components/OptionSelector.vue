<template>
    <div>
        <h4> {{id}} from {{origin}} </h4>
        <select :disabled="disabled"
                @change="handleChange">
            <option disabled selected value> -- select an option -- </option>
            <option v-for="option in options" :key="option">
                {{option}}
            </option>
        </select>
        <!--
        <select :disabled="disabled"
                @change="event => $emit('input',event.target.value)">
            <option v-for="option in options"
                    :key="option">
            {{option}}
            </option>
        </select>
        -->

    </div>
</template>


<script setup lang="ts">
    import { Ref, computed, ref, defineProps, defineEmits } from 'vue'
    import { IChoice } from '@/types';

    const emit = defineEmits<{
        (e: 'choice', choice: IChoice): void
    }>()

    const props = defineProps<{
        origin: string,
        id: string,
        options?: string[],
        selected?: string
    }>()

    const disabled: Ref<boolean> = ref(false)

    async function handleChange(event: Event) {
        emit('choice', {source: props.origin,
                        id: props.id,
                        choice: event.target.value
                       })
    }

    // const prompt = "<option disabled selected value> -- select an option -- </option>";
</script>