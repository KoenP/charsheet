<template>
  <div class="sidenav" id="sidenav">
    <button>test</button>
  </div>
  <div class="main">
    {{charOptions}}
    <div v-for="option in charOptions" :key="(option.origin,option.id)">
      <OptionSelector :origin="option.origin" :id="option.id" :options="option.spec.list"
                      @choice="api.registerChoice"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import { api } from './request'
import { Ref, reactive, ref, onMounted } from 'vue'
import { ICharacterOption, IChoice } from './types'
import OptionSelector from './components/OptionSelector.vue'
  // todo: suppress vs code error, this is correct

const charOptions: Ref<ICharacterOption[]> = ref([]) // should this be a reactive instead of a ref?
async function updateCharOptions(): Promise<void> {
  api.getPossibleCharacterOptions().then(options => charOptions.value = options)
}

// async function registerChoice(choice: IChoice) {
//   console.log(choice)
//   api.registerChoice(choice)
// }

onMounted(updateCharOptions)
</script>

<style>
.sidenav {
    height: 100%;
    width: 160px;
    position: fixed;
    z-index: 1;
    top: 0;
    left: 0;
    background-color: #111;
    overflow-x: hidden;
}

.sidenav button {
    background: none!important;
    border: none;
    margin-left: 20px;
    margin-top: 15px;
    padding: 0!important;
    /*optional*/
    /*font-family: arial, sans-serif; */
    /*input has OS specific font-family*/
    color: #818181;
    cursor: pointer;
    /* padding: 6px 8px 6px 16px; */
    font-size: 25px;
    display: block;
    transition-duration: 0.4s;
}

.sidenav button.selected {
    color: #ffffff;
    text-decoration: underline;
}

.sidenav button:hover {
    color: #ffffff;
}

.sidenav a {
    padding: 6px 8px 6px 16px;
    text-decoration: none;
    font-size: 25px;
    color: #818181;
    display: block;
}

.sidenav a:hover {
    color: #f1f1f1;
}

.main {
    margin-left: 160px;
    padding: 0px 10px;
}


</style>
