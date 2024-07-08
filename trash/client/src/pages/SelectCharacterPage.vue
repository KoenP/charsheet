<template>
    <div>
        Create a new character:
        <p>
            <input type="text" v-model="newCharName" placeholder="Enter new character name">
            <button @click="createNewCharacter(newCharName)"
                    :disabled="newCharName === ''">
                Create
            </button>
        </p>
    </div>

    <div>
        Or select a saved character:
        <ul>
            <li
                v-for="char in characterList"
                :key="char"
                class="charbtn"
            >
                <button @click="selectSavedCharacter(char)">{{char}}</button>
            </li>
        </ul>
    </div>
</template>

<script setup lang="ts">
    import { Ref, ref, onMounted } from 'vue'
    import { api } from '@/request'

    const characterList: Ref<string[]> = ref([])

    const newCharName: Ref<string> = ref("")
    
    async function selectSavedCharacter(char:string): Promise<void> {
        await api.selectSavedCharacter(char)
        window.location.href = 'edit'
    }

    async function createNewCharacter(name: string): Promise<void> {
        await api.createNewCharacter(name)
        window.location.href = 'edit'
    }

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