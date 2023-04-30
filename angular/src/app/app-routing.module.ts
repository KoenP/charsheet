import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { CharacterSelectionComponent } from './character-selection/character-selection.component';
import { SheetComponent } from './sheet/sheet.component';

const routes: Routes = [
  { path: 'character-selection', component: CharacterSelectionComponent },
  { path: 'sheet', component: SheetComponent }
]

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
