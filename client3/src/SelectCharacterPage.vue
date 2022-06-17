<template>
    <div>
        <button>Create a new character</button>
    </div>

    <div>
        Or select a saved character:
        <ul>
            <li
            v-for="char in characterList"
            :key="char"
            class="charbtn"
            >
                <button>{{char}}</button>
            </li>
        </ul>
    </div>
</template>

<script setup lang="ts">
    import { Ref, ref, onMounted } from 'vue'
    import { api } from './request'

    const characterList: Ref<string[]> = ref([])

    onMounted(() => api.listCharacters().then(chars => characterList.value = chars))
</script>

<style>
.charbtn button {
    background: none;
    border: none;
    cursor: pointer;
    display: block;
    text-decoration: underline;
}
</style>