import { ProgramakerPage } from './app.po';

describe('programaker App', () => {
  let page: ProgramakerPage;

  beforeEach(() => {
    page = new ProgramakerPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
