<template>
    <table>
        <tr>
            <th>Attribute</th>
            <th>Base value</th>
            <th>After bonuses</th>
            <th>Modifier</th>
        </tr>
        <template v-if="abilityTableData !== null">
            <tr v-for="abi in ['str','con','dex','int','wis','cha']"
                :key="abi">
                <th>{{abi}}</th>
                <td>
                    <input type="number"
                        :value="abilityTableData[abi]['base']"
                        :disabled="lock ? 'disabled' : null"
                        @change="event => $emit('updateBaseAbility', abi, event.target.value)">
                </td>
                <td>{{abilityTableData[abi]['score']}}</td>
                <td>{{formatModifier(abilityTableData[abi]['mod'])}}</td>
            </tr>
        </template>
    </table>
    <ul class="todo">
        <li>Looks like trash</li>
        <li>Fix flickering</li>
        <li>Updating this is currently extremely fragile. Check comment in EditCharacterPage.vue</li>
    </ul>
</template>

<script setup lang="ts">
    import { defineProps, defineEmits } from 'vue';
    import { Ability, AbilityTableData } from '@/types'
    import { formatModifier } from '@/util';

    const props = defineProps<{
        abilityTableData: AbilityTableData | null,
        lock: boolean
    }>()

    const emit = defineEmits<{
        (e:'updateBaseAbility', ability:Ability, value:number): void
    }>()
</script>