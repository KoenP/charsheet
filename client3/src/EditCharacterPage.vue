<template>
  <div class="sidenav" id="sidenav">
    <button
      v-for="level in levels"
      :key="level"
      :class="level === selectedLevel ? 'selected' : null"
      @click="selectedLevel = level"
    >
      Level {{level}}
    </button>
  </div>
  <div class="main">
    <div v-for="(options, category) in charOptionsAtCurrentLevelByCategory" :key="category">
      <h2>From {{category}}</h2>
        <div v-for="option in options" :key="(option.origin,option.id)">
          <OptionSelector :charoption="option" @choice="registerChoice"/>
        </div>
    </div>
  </div>
</template>

<script setup lang="ts">
  // :origin="option.origin" :id="option.id" :options="option.spec.list"
  import { api } from './request'
  import { Ref, ComputedRef, computed, reactive, ref, onMounted } from 'vue'
  import { ICharacterOption, IChoice } from './types'
  import { nub, sortNumbers, groupBy } from './util'
  import OptionSelector from '@/components/OptionSelector.vue'
  import ListSpec from '@/components/ListSpec.vue'
  import OrSelector from './components/OrSelector.vue'
    // todo: suppress vs code error, this is correct

  const charOptions: Ref<ICharacterOption[]> = ref([]) // should this be a reactive instead of a ref?
  async function updateCharOptions(): Promise<void> {
    api.getPossibleCharacterOptions().then(options => charOptions.value = options)
  }

  const levels = computed(() => 
    nub(sortNumbers(charOptions.value.map(opt => opt.charlevel))))
  const selectedLevel: Ref<number> = ref(1)

  const charOptionsAtCurrentLevelByCategory
    : ComputedRef<{[category: string]: ICharacterOption[]}>
    = computed(function() {
       const filteredByLevel = charOptions.value.filter(opt => opt.charlevel === selectedLevel.value)
       return groupBy(opt => opt.origin_category, filteredByLevel)
      })

  const charOptionsForSelectedLevel = computed(() => 
    charOptions.value.filter(opt => opt.charlevel === selectedLevel.value))

  async function registerChoice(choice: IChoice) {
    console.log(choice)
    await api.registerChoice(choice)
    await updateCharOptions()
  }

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
