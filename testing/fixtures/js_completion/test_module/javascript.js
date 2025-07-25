/** @odoo-module **/

export class TestWidget extends Component {
    static template = "test_module.TestWidget";

    async onButtonClick() {
        const result = await this.orm.call('test', 'test_method');
        //                                  ^complete test.model

        // Test method name completion
        const result2 = await this.orm.call('test.model', 'an');
        //                                                  ^complete another_method
    }
}
