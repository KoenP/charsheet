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
    <button
      @click="selectedLevel = 'level up'"
      :class="selectedLevel === 'level up' ? 'selected' : null"
    >
      +
    </button>
  </div>
  <div class="main">
    <h1>{{charName}}</h1>
    <p><a href="/">&lt;&lt; Back to character selection</a></p>
    <p><a href="sheet">Show character sheet</a></p>

    <template v-if="selectedLevel === 'level up'">
      Level up as 
      <select @change="event => gainLevel(event.target.value)">
        <option disabled selected value> -- select an option -- </option>
        <option v-for="option in classOptions" :key="option">{{option}}</option>
      </select>
      <ul class="todo">
        <li> Check multiclass requirements.  </li>
        <li> Default select base class or class of previous level.  </li>
      </ul>
    </template>

    <template v-else>
      <div v-if="selectedLevel === 1">
        <h2>Abilities</h2>
        <AbilityTable
          :abilityTableData="abilityTableData"
          @updateBaseAbility="registerBaseAbilityUpdate"
          :lock="lock"
        />
      </div>
      <div v-for="(options, category) in charOptionsAtCurrentLevelByCategory" :key="category">
        <h2>From {{category}}</h2>
          <div v-for="option in options" :key="(option.origin,option.id)">
            <OptionSelector :charoption="option" @choice="registerChoice"/>
          </div>
      </div>
    </template>

    <ul class="todo">
      <li>Don't update the whole edit page every time.</li>
    </ul>
  </div>
</template>

<script setup lang="ts">
  // :origin="option.origin" :id="option.id" :options="option.spec.list"
  import { api } from '@/request'
  import { Ref, ComputedRef, computed, reactive, ref, onMounted } from 'vue'
  import { ICharacterOption, IChoice, AbilityTableData, Ability } from '@/types'
  import { nub, sortNumbers, groupBy } from '@/util'
  import OptionSelector from '@/components/OptionSelector.vue'
  import ListSpec from '@/components/ListSpec.vue'
  import OrSelector from '@/components/OrSelector.vue'
  import AbilityTable from '@/components/AbilityTable.vue'

  const charName: Ref<string> = ref("")
  const charOptions: Ref<ICharacterOption[]> = ref([]) // should this be a reactive instead of a ref?
  const classOptions: Ref<string[]> = ref([])
  const abilityTableData: Ref<AbilityTableData | null> = ref(null)
  const lock: Ref<boolean> = ref(false)

  async function updateCharOptions(): Promise<void> {
    lock.value = true
    // TODO: look at this shit. In a different order the ability table data doesn't load. Why. Whywhywhywhywhy
    charOptions.value = await api.getPossibleCharacterOptions()
    classOptions.value = await api.listClassOptions()
    abilityTableData.value = await api.getAbilityTable()
    lock.value = false
  }

  const levels = computed(() => 
    nub(sortNumbers(charOptions.value.map(opt => opt.charlevel))))
  const selectedLevel: Ref<number | 'level up'> = ref(1)

  const charOptionsAtCurrentLevelByCategory
    : ComputedRef<{[category: string]: ICharacterOption[]}>
    = computed(function() {
       const filteredByLevel = charOptions.value.filter(opt => opt.charlevel === selectedLevel.value)
       return groupBy(opt => opt.origin_category, filteredByLevel)
      })

  const charOptionsForSelectedLevel = computed(() => 
    charOptions.value.filter(opt => opt.charlevel === selectedLevel.value))

  async function registerChoice(choice: IChoice) {
    await api.registerChoice(choice)
    await updateCharOptions()
  }

  async function registerBaseAbilityUpdate(ability: Ability, value: number): Promise<void> {
    await api.registerBaseAbilityUpdate(ability, value)
    await updateCharOptions()
  }

  async function gainLevel(className: string) {
    await api.gainLevel(className)
    await updateCharOptions()
    selectedLevel.value = levels.value.slice(-1)[0]
  }

  onMounted(async function () {
    charName.value = await api.getCurrentCharName()
    await updateCharOptions()
  })
</script>

<style>
.todo:before {
  content: "TODO: ";
}
.todo {
  color: gray;
  font-size: 8pt;
}

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

.main {
  margin-left: 160px;
  padding: 0px 10px;
}


</style>
