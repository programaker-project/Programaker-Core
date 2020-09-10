import { Component } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { AdminService, PlatformStatsInfo, UserAdminData } from 'app/admin.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { unixMsToStr } from 'app/utils';

interface Note {
    icon: string;
    text: string;
}

const SYSTEM_STAT_RELOAD_TIME = 15000; // 15 seconds

@Component({
    // moduleId: module.id,
    selector: 'app-my-admin-settings',
    templateUrl: './admin-settings.component.html',
    providers: [AdminService, SessionService],
    styleUrls: [
        'admin-settings.component.css',
        '../../libs/css/material-icons.css',
        '../../libs/css/bootstrap.min.css',
    ],
})
export class AdminSettingsComponent {
    session: Session;
    users: UserAdminData[];
    notes: { [key: string]: Note[] } = {};
    stats: PlatformStatsInfo;
    serviceNames: string[] = [];

    constructor(
        public adminService: AdminService,
        public sessionService: SessionService,
        public router: Router,
        public dialog: MatDialog,
    ) {
    }

    // tslint:disable-next-line:use-life-cycle-interface
    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (!session.active) {
                    this.router.navigate(['/login'], {replaceUrl:true});
                }
                else if (!session.tags.is_admin) {
                    this.router.navigate(['/settings'], {replaceUrl:true});
                }

                this.adminService.listAllUsers().then(users => {
                    this.users = users.sort((x, y) => {
                        // Note that this sorting is reversed
                        if (x.registration_time > y.registration_time) {
                            return -1;
                        }
                        else if (y.registration_time > x.registration_time) {
                            return 1;
                        }
                        return 0;
                    } );

                    for (const user of users) {
                        this.annotate(user);
                    }
                });

                const reload_stats = () => this.adminService.getStats().then(stats => {
                    this.stats = stats;
                    this.serviceNames = Object.keys(stats.stats.active_services).sort();
                    setTimeout(reload_stats, SYSTEM_STAT_RELOAD_TIME);
                }).catch(err => {
                    console.error("Error loading stats:", err);
                });
                reload_stats();
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            });
    }

    annotate(user: UserAdminData) {
        // Check username
        const notes: Note[] = [];

        if ((user.username.length < 4) || (user.username.length > 50)){
            notes.push({
                icon: "warning", // This is not allowed, but can be handled
                text: "User name should have at least 4 and at most 50 characters",
            });
        }
        if (!user.username.match(/^[_a-zA-Z0-9]*$/)) {
            notes.push({
                icon: "error",
                text: "User name can only contain letters (a-z), digits (0-9) and underscores (_)",
            });
        }
        if (user.status !== 'ready') {
            notes.push({
                icon: "warning",
                text: "User status: " + user.status,
            });
        }

        this.notes[user.user_id] = notes;
    }

    getLocalRegistrationTime(user: UserAdminData): string {
        return unixMsToStr(user.registration_time * 1000, { ms_precision: false });
    }

    getLocalLastActiveTime(user: UserAdminData): string {
        if (!user.last_active_time) {
            return 'none';
        }
        return unixMsToStr(user.last_active_time * 1000, { ms_precision: false });
    }
}
