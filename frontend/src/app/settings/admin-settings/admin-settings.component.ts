import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Router, ActivatedRoute } from '@angular/router';
import { AdminService, PlatformStatsInfo, UserAdminData } from 'app/admin.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { unixMsToStr } from 'app/utils';
import { isPlatformBrowser } from '@angular/common';

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
        private route: ActivatedRoute,
        public dialog: MatDialog,
        @Inject(PLATFORM_ID) private platformId: Object
    ) {
        this.route.data
            .subscribe({
                next: (data: { session: Session, adminStats: PlatformStatsInfo, userList: UserAdminData[] }) => {
                    this.session = data.session;
                    if (!data.session.active) {
                        this.router.navigate(['/login'], {replaceUrl:true});
                        return;
                    }
                    else if (!data.session.tags.is_admin) {
                        this.router.navigate(['/settings'], {replaceUrl:true});
                        return;
                    }

                    this.stats = data.adminStats;
                    this.serviceNames = Object.keys(this.stats.stats.active_services).sort();

                    for (const user of data.userList) {
                        this.annotate(user);
                    }

                    this.users = data.userList.sort((x, y) => {
                        // Note that this sorting is reversed
                        if (x.registration_time > y.registration_time) {
                            return -1;
                        }
                        else if (y.registration_time > x.registration_time) {
                            return 1;
                        }
                        return 0;
                    } );
                },
                error: err => {
                    console.log('Error getting session', err);
                    this.router.navigate(['/login'], {replaceUrl:true});
                }
            });

    }

    // tslint:disable-next-line:use-life-cycle-interface
    ngOnInit(): void {
        // Don't enter an infinite loop when rendering on server
        if (isPlatformBrowser(this.platformId)) {
            setTimeout(this.reload_stats.bind(this), SYSTEM_STAT_RELOAD_TIME);
        }
    }

    reload_stats() {
        this.adminService.getStats().then(stats => {
            this.stats = stats;
            this.serviceNames = Object.keys(stats.stats.active_services).sort();

            // Don't enter an infinite loop when rendering on server
            if (isPlatformBrowser(this.platformId)) {
                setTimeout(this.reload_stats.bind(this), SYSTEM_STAT_RELOAD_TIME);
            }
        }).catch(err => {
            console.error("Error loading stats:", err);
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
