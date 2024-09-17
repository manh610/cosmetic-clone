package com.cosmetic.gg.entity;

import java.io.Serializable;
import java.time.LocalDateTime;

import javax.persistence.*;

import org.hibernate.annotations.GenericGenerator;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.utils.SecurityUserUtils;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
@MappedSuperclass
public class EntityCommon implements Serializable{

	private static final String USER_SYSTEM = "system";
	
	@Id
	@Column(name = "id", nullable = false, updatable = false)
	@GeneratedValue(generator = "uuid2")
	@GenericGenerator(name = "uuid2", strategy = "uuid2")
	private String id;
	
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private EStatus status;
	
	@Column(name = "created_at", updatable = false)
	private LocalDateTime createdAt = LocalDateTime.now();
	
	@Column(name = "created_by", updatable = false)
	private String createdBy = USER_SYSTEM;
	
	@Column(name = "updated_at")
	private LocalDateTime updatedAt = LocalDateTime.now();
	
	@Column(name = "updated_by")
	private String updatedBy = USER_SYSTEM;
	
	@Column(name = "description")
	private String description;
	
	public void prepareEntity(String username, EStatus status) {
		if (status == null)
			status = EStatus.ACTIVE;
		
		this.setStatus(status);
		this.setCreatedBy(username);
		this.setCreatedAt(LocalDateTime.now());
		this.setUpdatedBy(username);
		this.setUpdatedAt(LocalDateTime.now());
	}
	
	public void prepareEntity(EStatus status) {
		String username = SecurityUserUtils.getCurrentUser() == null ?
                USER_SYSTEM : SecurityUserUtils.getCurrentUser().getUsername();
		this.setStatus(status);
		this.setCreatedBy(username);
		this.setCreatedAt(LocalDateTime.now());
		this.setUpdatedBy(username);
		this.setUpdatedAt(LocalDateTime.now());
	}
	
	public void prepareEntity() {
		String username = SecurityUserUtils.getCurrentUser() == null ?
                USER_SYSTEM : SecurityUserUtils.getCurrentUser().getUsername();
		this.setStatus(EStatus.ACTIVE);
		this.setCreatedBy(username);
		this.setCreatedAt(LocalDateTime.now());
		this.setUpdatedBy(username);
		this.setUpdatedAt(LocalDateTime.now());
	}
	
	public void prepareEntityV2() {
		String username = SecurityUserUtils.getCurrentUser() == null ?
                USER_SYSTEM : SecurityUserUtils.getCurrentUser().getUsername();
		this.setCreatedBy(username);
		this.setCreatedAt(LocalDateTime.now());
		this.setUpdatedBy(username);
		this.setUpdatedAt(LocalDateTime.now());
	}
}
