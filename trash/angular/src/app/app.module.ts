import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { CharacterSelectionComponent } from './character-selection/character-selection.component';
import { SheetComponent } from './sheet/sheet.component';
import { SpellPreparedComponent } from './sheet/spell-prepared/spell-prepared.component';

@NgModule({
  declarations: [
    AppComponent,
    CharacterSelectionComponent,
    SheetComponent,
    SpellPreparedComponent
  ],
  imports: [
    AppRoutingModule,
    BrowserModule,
    HttpClientModule,
    FormsModule,
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
