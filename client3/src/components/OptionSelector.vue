<template>
    <div>
        <h4> {{charoption.id}} from {{charoption.origin}} </h4>
        <ListSpec></ListSpec>
        <ListSpec />
        
        <!-- ListSpecComponent disabled="false" selectedValue="undefined" :options="charoption.spec.list" /-->
    </div>
</template>

<script setup lang="ts">
    import { Ref, computed, ref, defineProps, defineEmits } from 'vue'
    import { IChoice, ICharacterOption } from '@/types';
    // import { ListSpecComponent } from './ListSpecComponent.vue'
    import { ListSpec } from '@/components/ListSpec.vue';

    // const emit = defineEmits<{
    //     (e: 'choice', choice: IChoice): void
    // }>()

    const props = defineProps<{
        charoption: ICharacterOption
    }>()

    const disabled: Ref<boolean> = ref(false)

    // async function handleChange(event: Event) {
    //     emit('choice', {source: props.charoption.origin,
    //                     id: props.charoption.id,
    //                     choice: event.target.value
    //                    })
    // }

    // const prompt = "<option disabled selected value> -- select an option -- </option>";
</script>