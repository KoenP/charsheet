<template>
    <div>
        <h4> {{charoption.id}} from {{charoption.origin}} </h4>
        <SubSelector
            :spec="charoption.spec"
            :selected="(charoption.choice as Selection | null)"
            :disabled="disabled"
            @choice="selection =>
                $emit('choice',
                      {source: charoption.origin,
                       id: charoption.id,
                       choice: selection})"
        />
    </div>
</template>

<script setup lang="ts">
    import { ComputedRef, Ref, computed, ref, defineProps, defineEmits } from 'vue'
    import { IChoice, ICharacterOption } from '@/types';
    import ListSpec from '@/components/ListSpec.vue';
    import SubSelector from './SubSelector.vue';

    const emit = defineEmits<{
        // choice: Selection | null
        (e:'choice', choice:IChoice): void
    }>()

    const props = defineProps<{
        charoption: ICharacterOption
    }>()

    const disabled: Ref<boolean> = ref(false)

    function typecastSelectedValue(value: string | null): string | undefined {
        return value as string | undefined
    }

    // async function handleChange(event: Event) {
    //     emit('choice', {source: props.charoption.origin,
    //                     id: props.charoption.id,
    //                     choice: event.target.value
    //                    })
    // }

    // const prompt = "<option disabled selected value> -- select an option -- </option>";
</script>